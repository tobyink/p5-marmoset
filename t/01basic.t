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
	
	has foo => (is => 'ro', pack => 'Z16', default => sub { "Hello" });
	has bar => (is => 'rw', pack => 'Z16', required => 1, isa => Str);
	has baz => (is => 'rw');	
}

object_ok(
	sub { Local::Example->new(bar => "World", baz => "!!!") },
	'$eg',
	isa   => [qw/ Local::Example Marmoset::Object /],
	does  => [qw/ Local::Example Marmoset::Object /],
	can   => [qw/ new foo bar baz /],
	more  => sub {
		my $eg = shift;
		
		is($eg->foo, 'Hello', '$eg->foo');
		is($eg->bar, 'World', '$eg->bar');
		is($eg->baz, '!!!', '$eg->baz');
		
		$eg->bar("There");
		$eg->baz("???");
		
		is($eg->foo, 'Hello', '$eg->foo');
		is($eg->bar, 'There', '$eg->bar');
		is($eg->baz, '???', '$eg->baz');
		
		(my $str = $$eg) =~ s/\0/./g;
		
		is($str, 'Hello...........There...........', '$$eg');
	},
);

done_testing;

