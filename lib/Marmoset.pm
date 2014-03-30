use 5.008003;
use strict qw( subs vars );
use warnings;
no warnings qw( once uninitialized );

package Marmoset;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.000_01';

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

sub make_immutable
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
	push @code, '      my $new = "Marmoset"->make_immutable($class);';
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
	our $AUTHORITY = 'cpan:TOBYINK';
	our $VERSION   = '0.000_01';
	
	sub new
	{
		my ($class) = @_;
		my $new = 'Marmoset'->make_immutable($class);
		goto $new;
	}
	
	sub BUILDARGS
	{
		shift;
		+{
			(@_==1 and ref($_[0]) eq q(HASH)) ? %{$_[0]} : @_
		};
	}
	
	sub DESTROY { return; }
}

{
	package Marmoset::Attribute;
	our $AUTHORITY = 'cpan:TOBYINK';
	our $VERSION   = '0.000_01';
	our @ISA       = qw(Sub::Accessor::Small);
	
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
	our $AUTHORITY = 'cpan:TOBYINK';
	our $VERSION   = '0.000_01';
	our @ISA       = qw(Marmoset::Attribute);
	
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
	our $AUTHORITY = 'cpan:TOBYINK';
	our $VERSION   = '0.000_01';
	our @ISA       = qw(Marmoset::Attribute);
	
	sub accessor_kind { 'Marmoset inside-out' }
}


1;

__END__

=pod

=encoding utf-8

=for stopwords featureful

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

=head2 Keywords provided by Marmoset

=over

=item C<< extends(@classes) >>

Set inheritance for the current class. Currently you may only inherit
from other Marmoset classes.

If you don't specify a class to inherit from, your class will inherit
from Marmoset::Object. (See L</"Methods provided by Marmoset::Object">.)

=item C<< has $attribute => %specification >>

Creates an attribute for your class, using a Moose-like specification.
There is a convention that attributes named with a leading underscore
are undocumented, unsupported or "private". (see L<Lexical::Accessor>
for true private attributes though.)

The following keys are supported in the specification hash:

=over

=item C<< is => "ro"|"rw"|"rwp"|"lazy" >>

Shortcuts for common patterns of accessors. As documented in
L<Moo> and L<MooseX::AttributeShortcuts>.

=item C<< pack => $template >>

The presence of this key in the specification makes your attribute
be stored in the object's packed string.

Attributes suitable for packed storage include numbers and small
(especially fixed-length) strings.

Templates are as defined in L<perlfunc/"pack">.

=item C<< reader => $name|1 >>

Specify the name for a read-only accessor method. Passing the
value "1" names the accessor "get_${attribute}" (for "private"
attributes, "_get${attribute}").

=item C<< writer => $name|1 >>

Specify the name for a write-only accessor method. Passing the
value "1" names the accessor "set_${attribute}" (for "private"
attributes, "_set${attribute}").

=item C<< accessor => $name|1 >>

Specify the name for a read/write accessor method. Passing the
value "1" names the accessor "${attribute}".

=item C<< predicate => $name|1 >>

Specify the name for a predicate method. Passing the value "1"
names the predicate "has_${attribute}" (for "private" attributes,
"_has${attribute}").

For any attributes which are stored packed, it makes little
sense to define a predicate. The predicate will always return
true.

=item C<< clearer => $name|1 >>

Specify the name for a clearer method. Passing the value "1"
names the predicate "clear_${attribute}" (for "private" attributes,
"_clear${attribute}").

For any attributes which are stored packed, it makes little
sense to define a clearer. The clearer will always throw an
exception.

=item C<< trigger => $coderef|$name|1 >>

A coderef to call after the value for the attribute has been set.
Alternatively, a method name may be supplied. Passing the value
"1" is equivalent to the method name "_trigger_${attribute}".

=item C<< builder => $coderef|$name|1 >>

A method name to call to build a default value for the attribute.
Passing the value "1" is equivalent to the method name
"_build_${attribute}". If a coderef is supplied, this will be
installed into the class as "_build_${attribute}".

=item C<< default => $coderef|$nonref >>

Similar to C<builder>, but the coderef will not be installed as
a class method. Non-reference values (i.e. undef, numbers, strings),
may be supplied as a simple value instead of a coderef.

=item C<< isa => $constraint|$coderef|$typename >>

A type constraint to validate values for the attribute. Any
constraint object which meets the L<Type::API::Constraint> specification
can be provided, including L<Type::Tiny>, L<MooseX::Types>,
L<MouseX::Types>, and L<Specio> type constraint objects.

Alternatively a validation coderef may be provided, which must
return true to indicate a valid value, and either return false or
throw an exception to indicate an invalid one.

If L<Type::Utils> is installed, this may be provided as a string,
which will be expanded to a type constraint object using C<dwim_type>.
(See L<Type::Utils/"dwim_type">.)

=item C<< does => $role >>

Shorthand for C<< isa => ConsumerOf[$role] >>.

=item C<< coerce => $coercion|$coderef|0|1 >>

Indicates whether type coercion should be attempted before validating
values.

If an object meeting the L<Type::API::Constraint::Coercible>
specification has been provided for C<isa>, then the value "1" will
reuse any coercion attached to the type constraint object.

Otherwise, an type coercion object (i.e. with a C<coerce> method) may
be provided, or a coderef which accepts a value and returns the coerced
value.

=item C<< handles => $arrayref|$hashref >>

Delegates methods to the attribute value.

=item C<< weak_ref => 0|1 >>

Indicates whether the attribute value should be weakened. Only makes
sense for attributes which are not stored packed.

=item C<< init_arg => $name|undef >>

The named constructor argument that will provide an initial value for
the attribute. If omitted, will default to $attribute.

=item C<< required => 0|1 >>

Indicates whether it is necessary to set the attribute when constructing
objects.

Attributes which are stored packed I<must> be required unless they
provide a default/builder.

=back

=back

=head2 Keywords NOT provided by Marmoset

Unlike L<Moose>, L<Mouse>, and L<Moo>, Marmoset does not provide native
support for method modifiers or roles. Instead, it recommends the use of
L<Class::Method::Modifiers> and L<Role::Tiny> respectively.

Note that Marmoset is sometimes forced to rebuild constructors and
accessors at run-time, which may lead to your method modifiers being
overwritten, if you have tried to apply any modifiers to them.

It may be useful to force Marmoset to perform its rebuilding early; after
you've finished defining your class' inheritance and attributes, but
before applying any roles or method modifiers. To do this, call:

   Marmoset->make_immutable(__PACKAGE__);

=begin trustme

=item make_immutable

=end trustme

=head2 Methods provided by Marmoset::Object

Marmoset::Object is your object's base class. It provides the
following methods:

=over

=item C<< new(%attributes) >>

Your class' constructor.

=item C<< BUILDARGS(@args) >>

This is the proper way to alter incoming arguments to get them into a
format that Marmoset::Object's default constructor will recognize. This
class method is passed the list of constructor arguments as-is, and
expected to return a hashref of parameters which will be used to
initialize attributes.

=item C<< BUILD($parameters) >>

(Actually Marmoset::Object doesn't provide this, but your class may.)

This is the proper way to perform any additional initialization on
your objects. It is called as an object method. If you're inheriting
from another Marmoset class, you I<< must not >> call
C<< $self->SUPER::BUILD(@_) >>. Marmoset will do that for you!

=item C<< DESTROY >>

TODO - not implemented

=item C<< DEMOLISH >>

TODO - not implemented

=back

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Marmoset>.

=head1 SEE ALSO

L<Moose>,
L<Moo>,
L<Mouse>,
L<Class::Tiny>.

L<http://www.perlmonks.org/?node_id=1040313>.

L<http://www.flickr.com/photos/tambako/10655212644/>.

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

