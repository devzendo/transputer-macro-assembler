#!/usr/bin/perl
#
# Extract all addresses, in order, from the end of the .LST file to give the
# symbol table that the emulator can read.
# (eventually, add this as a tmasm option to generate directly)
#
use warnings;
use strict;

die "gen-sym.pl FILE.LST\nGenerates a FILE.SYM symbol table for reading by the
emulator, from an input TMASM listing\n" unless (scalar(@ARGV) == 1);
my $lst = $ARGV[0];
die "Argument must be a listing file\n" unless ($lst =~ /\.lst$/i);

open(my $fh, "<", $lst) or die "Can't open $lst: $!\n";
my $sym = $lst;
$sym =~ s/\.lst$/.sym/;
$sym =~ s/\.LST$/.SYM/;
open(my $ofh, ">", $sym) or die "Can't create $sym: $!\n";
my $found_symbol_table = 0;
my @address_lines = ();
while (<$fh>) {
  chomp;
  if (/^Symbol Table - by Address/) {
    $found_symbol_table = 1;
  }
  next unless $found_symbol_table;
  
  if ($_ =~ /^(\S+)\s+([0-9A-F]+)$/) {
    push @address_lines, $_ if $_;
  }
}
close $fh;

foreach (@address_lines) {
  print $ofh "$_\n";
}
close $ofh;

print "Created $sym with " . scalar(@address_lines) . " symbol(s)\n";
