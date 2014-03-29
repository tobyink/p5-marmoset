use 5.008003;
use strict qw( subs vars );
use warnings;
no warnings qw( once uninitialized );

package Marmoset;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.001';

our (%ATTRIBUTES, %IS_FINAL, %OFFSETS, %SIZES);

sub _class_for_objects    { 'Marmoset::Object' }
sub _class_for_fields     { 'Marmoset::Attribute::Field' }
sub _class_for_attributes { 'Marmoset::Attribute::InsideOut' }

use B qw();
use Carp qw(croak);
use Eval::TypeTiny qw(eval_closure);
use Exporter::Shiny our(@EXPORT) = qw( has extends );
use Sub::Accessor::Small 0.008 ();

BEGIN {
	($] >= 5.010) ? do { require mro } : do { require MRO::Compat };	
}

sub _exporter_validate_opts
{
	my $me = shift;
	my ($globals) = @_;
	my $caller = $globals->{into};
	
	ref($caller)
		and croak("Cannot import Marmoset into a reference; bailing out");
	
	@{"$caller\::ISA"} = $me->_class_for_objects;
}

sub _generate_has
{
	my $me = shift;
	my ($name, undef, $globals) = @_;
	my $caller = $globals->{into};
	
	return sub
	{
		$IS_FINAL{$caller}
			and croak("$caller has already been instantiated; cannot add attributes; bailing out");
		
		my ($name, %opts) = @_;
		my $attr_class = exists($opts{pack})
			? $me->_class_for_fields
			: $me->_class_for_attributes;
		
		$attr_class->has($globals, $name, %opts);
	};
}

sub _generate_extends
{
	my $me = shift;
	my ($name, undef, $globals) = @_;
	my $caller = $globals->{into};
	
	my $need = $me->_class_for_objects;
	
	return sub
	{
		$IS_FINAL{$caller}
			and croak("$caller has already been instantiated; cannot change inheritance; bailing out");
		
		my @parents = @_;
		$_->isa($need)
			or croak("Cannot inherit from $need; it is not a $me class (does not inherit from $need); bailing out")
			for @parents;
		@{"$caller\::ISA"} = @parents;
	};
}

sub _finalize_class
{
	my $me = shift;
	my ($class) = @_;
	$IS_FINAL{$_} = 1 for @{mro::get_linear_isa($class)};
	$me->_initialize_slots($class);
	return $me->_build_constructor($class);
}

sub _initialize_slots
{
	my $me = shift;
	my ($class) = @_;
	
	my @fields =
		grep exists($_->{pack}),
		map @{ $ATTRIBUTES{$_} or [] },
		@{mro::get_linear_isa($class)};
	
	my ($n, @sizes, @offsets) = 0;
	$sizes[@sizes] = $_,
	$offsets[@offsets] = $n,
	$n += $_
		for map length(pack $_->{pack}, 0), @fields;
	
	for my $i (0 .. $#fields)
	{
		local $Marmoset::REALLY_INSTALL = 1;
		my $f = $fields[$i];
		
		(my($tmp), $f->{package}) = ($f->{package}, $class);
		($f->{OFFSET}, $f->{SIZE}) = ($offsets[$i], $sizes[$i]);
		
		$f->install_accessors;
		
		delete($f->{$_}) for qw( OFFSET SIZE );
		$f->{package} = $tmp;
	}
}

sub _build_constructor
{
	my $me = shift;
	my ($class) = @_;
	
	my @isa = @{mro::get_linear_isa($class)};
	my @attr = map @{ $ATTRIBUTES{$_} or [] }, @isa;
	my @fields = grep exists($_->{pack}), @attr;
	my $template = B::perlstring( join q[ ], map $_->{pack}, @fields );
	my @others = grep !exists($_->{pack}), @attr;
	my @build_methods =
		map "$_\::BUILD",
		grep exists(&{"$_\::BUILD"}),
		@isa;
	my %defaults = map {
		exists($_->{default}) ? ($_->{slot}, $_->{default}) : ()
	} @attr;
	my %triggers = map {
		exists($_->{trigger}) ? ($_->{slot}, $_->{trigger}) : ()
	} @attr;
	my %types = map {
		exists($_->{isa}) ? ($_->{slot}, $_->{isa}) : ()
	} @attr;
	
	my @code;
	push @code, 'sub {';
	push @code, '   my $class = shift;';
	push @code, '   if ($class ne '.B::perlstring($class).') {';
	push @code, '      my $new = "Marmoset"->_finalize_class($class);';
	push @code, '      goto $new;';
	push @code, '   }';
	push @code, '   my $str = "";';
	push @code, '   my $self = \$str;';
	push @code, '   bless $self, $class;';
	push @code, '   my $params = $class->BUILDARGS(@_);';
	push @code, '   my (@f, $tmp);';
	for my $f (@fields) {
		push @code, '   if (exists($params->{'.B::perlstring($f->{init_arg}).'})) {';
		push @code, '      $$tmp = $params->{'.B::perlstring($f->{init_arg}).'};';
		push @code, '      $triggers{'.B::perlstring($f->{slot}).'}->($self, $params->{'.B::perlstring($f->{init_arg}).'});'
			if $triggers{$f->{slot}};
		push @code, '   }';
		push @code, '   else {';
		if ($f->{default} and not $f->{lazy}) {
			push @code, '      $$tmp = scalar($defaults{'.B::perlstring($f->{slot}).'}->($self));';
		}
		elsif ($f->{builder} and not $f->{lazy}) {
			push @code, '      $$tmp = scalar($self->'.$f->{builder}.'());';
		}
		else {
			push @code, '      Carp::croak(sprintf "Required parameter \\"%s\\" not supplied to constructor; stopped", '.B::perlstring($f->{init_arg}).');';
		}
		push @code, '   }';
		
		if (my $type = $f->{isa})
		{
			if ($f->{coerce})
			{
				push @code, '   $$tmp = '.(
					$type->coercion->can_be_inlined
						? $type->coercion->inline_coercion('${$tmp}')
						: '$types{'.B::perlstring($f->{slot}).'}->coerce($$tmp)'
				).';';
			}
			push @code, '   '.(
				$type->can_be_inlined
					? $type->inline_assert('${$tmp}')
					: ('$types{'.B::perlstring($f->{slot}).'}->assert_value($$tmp)')
			).';';
		}
		
		push @code, '   push @f, $$tmp;';
		push @code, '   undef $tmp;';
	}
	push @code, '   $str = pack('.$template.', @f);';
	push @code, '   @_ = $self;' if @others;
	for my $f (@others) {
		push @code, '   if (exists($params->{'.B::perlstring($f->{init_arg}).'})) {';
		push @code, '      $$tmp = $params->{'.B::perlstring($f->{init_arg}).'};';
		push @code, '      $triggers{'.B::perlstring($f->{slot}).'}->($self, $params->{'.B::perlstring($f->{init_arg}).'});'
			if $triggers{$f->{slot}};
		push @code, '   }';
		push @code, '   else {';
		if ($f->{default} and not $f->{lazy}) {
			push @code, '      $$tmp = scalar($defaults{'.B::perlstring($f->{slot}).'}->($self));';
		}
		elsif ($f->{builder} and not $f->{lazy}) {
			push @code, '      $$tmp = scalar($self->'.$f->{builder}.'());';
		}
		elsif ($f->{required}) {
			push @code, '      Carp::croak(sprintf "Required parameter \\"%s\\" not supplied to constructor; stopped", '.B::perlstring($f->{init_arg}).');';
		}
		else {
			push @code, '      ();';
		}
		push @code, '   }';
		
		if (my $type = $f->{isa})
		{
			push @code, '   if ($tmp) {';
			if ($f->{coerce})
			{
				push @code, '      $$tmp = '.(
					$type->coercion->can_be_inlined
						? $type->coercion->inline_coercion('${$tmp}')
						: '$types{'.B::perlstring($f->{slot}).'}->coerce($$tmp)'
				).';';
			}
			push @code, '      '.(
				$type->can_be_inlined
					? $type->inline_assert('${$tmp}')
					: ('$types{'.B::perlstring($f->{slot}).'}->assert_value($$tmp)')
			).';';
			push @code, '   }';
		}
		
		push @code, '   '.$f->inline_access.' = $$tmp if $tmp;';
		push @code, '   undef $tmp;';
	}
	push @code, "   \$self->$_\(\$params);" for reverse @build_methods;
	push @code, '   $self;';
	push @code, '}';
	
	my $coderef = eval_closure(
		source      => join("\n", @code),
		environment => {
			'%defaults' => \%defaults,
			'%triggers' => \%triggers,
			'%types'    => \%types,
		},
	);
	
	*{"$class\::new"} = Marmoset::Attribute->HAS_SUB_NAME
		? Sub::Name::subname("$class\::new", $coderef)
		: $coderef;
	return $coderef;
}

{
	package Marmoset::Object;
	
	sub new
	{
		my ($class) = @_;
		my $new = 'Marmoset'->_finalize_class($class);
		goto $new;
	}
	
	sub BUILDARGS
	{
		shift;
		+{
			(@_==1 and ref($_[0]) eq q(HASH)) ? %{$_[0]} : @_
		};
	}
}

{
	package Marmoset::Attribute;
	our @ISA = qw(Sub::Accessor::Small);
	
	sub accessor_kind { 'Marmoset' }
	
	sub canonicalize_opts
	{
		my $me = shift;
		
		$me->SUPER::canonicalize_opts(@_);
		$me->{init_arg} = $me->{slot} unless exists $me->{init_arg};
		
		# Save options
		push @{ $ATTRIBUTES{$me->{package}} ||= [] }, $me;
	}
}

{
	package Marmoset::Attribute::Field;
	our @ISA = qw(Marmoset::Attribute);
	
	use Carp qw(croak);	
	
	sub accessor_kind { 'Marmoset packed' }
	
	sub canonicalize_opts
	{
		my $me = shift;
		$me->SUPER::canonicalize_opts(@_);
		
		croak "Attribute '$me->{slot}' is a field, therefore must be required or have a default; bailing out"
			if exists($me->{required})
			&& !$me->{required}
			&& !$me->{builder}
			&& !$me->{default};
		
		croak "Attribute '$me->{slot}' is a field, therefore cannot be cleared; bailing out"
			if defined($me->{clearer});
	}
	
	sub inline_clearer
	{
		croak "This class cannot generate a clearer; bailing out";
	}
	
	sub inline_predicate
	{
		return q[ 1 ];
	}
	
	sub inline_access
	{
		my $me = shift;
		
		sprintf(
			'unpack(q(%s), substr(${$_[0]}, %d, %d))',
			$me->{pack},
			$me->{OFFSET} || 0,
			$me->{SIZE} || 0,
		);
	}
	
	sub inline_access_w
	{
		my $me = shift;
		my ($expr) = @_;
		
		sprintf(
			'substr(${$_[0]}, %d, %d) = pack(q(%s), %s)',
			$me->{OFFSET} || 0,
			$me->{SIZE} || 0,
			$me->{pack},
			$expr,
		);
	}
	
	sub install_accessors
	{
		return unless $Marmoset::REALLY_INSTALL;
		shift->SUPER::install_accessors(@_);
	}
}

{
	package Marmoset::Attribute::InsideOut;
	our @ISA = qw(Marmoset::Attribute);	
	sub accessor_kind { 'Marmoset inside-out' }
}


1;

__END__

=pod

=encoding utf-8

=head1 NAME

Marmoset - class builder for memory-efficient objects

=head1 SYNOPSIS

	use v5.14;
	use warnings;
	
	package MyClass {
		use Marmoset;
		has id   => (is => 'ro', pack => 'L');
		has name => (is => 'rw', pack => 'Z32');
	}
	
	my $obj = MyClass->new(id => 42, name => "The Answer");

=head1 DESCRIPTION

=begin html

<p
	id="#eating-marmoset-ii"
	style="float:right">
	<a
		href="http://www.flickr.com/photos/tambako/10655212644/"
		title="Eating marmoset II by Tambako the Jaguar, on Flickr">
		<img
			alt="Eating marmoset II"
			src="http://farm4.staticflickr.com/3834/10655212644_ae115ce604_n.jpg"
			width="213" height="320">
	</a>
</p>

=end html

B<Marmoset> is a slightly less featureful version of class builders
like L<Moose>, L<Mouse>, and L<Moo>, designed for efficient memory
usage when you need to deal with many thousands of very simple objects.

Attributes are stored using a variation on the C<pack>/C<unpack> shown
by L<BrowserUK|http://www.perlmonks.org/?node_id=171588> on PerlMonks
at L<http://www.perlmonks.org/?node_id=1040313>.

However, inside-out attributes are also offered for data which cannot
be reasonably serialized to a string.

=head2 Keywords

=over

=item C<< extends >>

=item C<< has >>

=back

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Marmoset>.

=head1 SEE ALSO

L<Moose>,
L<Moo>,
L<Mouse>,
L<Class::Tiny>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

