=pod

=encoding utf-8

=head1 PURPOSE

Basic checks for the Marmoset idea.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

use strict;
use warnings;
use Test::Modern;

{
	package Local::Example;
	use Marmoset;
	use Types::Standard -types;
	
	has foo => (
		is        => 'ro',
		pack      => 'Z16',
		default   => sub { "Hello" },
	);
	
	has bar => (
		is        => 'rw',
		pack      => 'Z16',
		required  => 1,
		isa       => Str->plus_coercions( ArrayRef, sub { join(q[|], @$_) } ),
		coerce    => 1
	);
	
	has baz => (
		is        => 'rw',
		predicate => "has_baz",  # XXX: TODO predicate => 1
	);
}

object_ok(
	sub { Local::Example->new(bar => "World", baz => "!!!") },
	'$eg',
	isa   => [qw/ Local::Example Marmoset::Object /],
	does  => [qw/ Local::Example Marmoset::Object /],
	can   => [qw/ new foo bar baz has_baz /],
	more  => sub {
		my $eg = shift;
		
		is($eg->foo, 'Hello', '$eg->foo');
		is($eg->bar, 'World', '$eg->bar');
		is($eg->baz, '!!!', '$eg->baz');
		ok($eg->has_baz, '$eg->has_baz');
		
		$eg->bar("There");
		$eg->baz("???");
		
		is($eg->foo, 'Hello', '$eg->foo');
		is($eg->bar, 'There', '$eg->bar');
		is($eg->baz, '???', '$eg->baz');
		ok($eg->has_baz, '$eg->has_baz');
		
		is_string($$eg, "Hello\0\0\0\0\0\0\0\0\0\0\0There\0\0\0\0\0\0\0\0\0\0\0", '$$eg');
	},
);

object_ok(
	sub { Local::Example->new(bar => [qw/1 2 3/], foo => "Yeah") },
	'$eg2',
	more  => sub {
		my $eg2 = shift;
		
		is($eg2->foo, 'Yeah', '$eg2->foo');
		is($eg2->bar, '1|2|3', '$eg2->bar (coerced value)');
		is($eg2->baz, undef, '$eg2->baz');
		ok(!$eg2->has_baz, '$eg2->has_baz');
		
		is_string($$eg2, "Yeah\0\0\0\0\0\0\0\0\0\0\0\x{0}1|2|3\0\0\0\0\0\0\0\0\0\0\0", '$$eg2');
	},
);

like(
	exception { Local::Example->new(bar => {}); },
	qr{did not pass type constraint},
	'type constraints enforced',
);

done_testing;

