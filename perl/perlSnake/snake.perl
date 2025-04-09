#!/usr/bin/perl

# ---------- imports ----------

use strict;
use warnings;
use Term::ReadKey;

# ---------- variable initialisation ----------

my $width = 20;
my $height = 10;
my $snake_char = 'O';
my $fruit_char = '@';
my $snake_speed = 0.1;
my $quit_key = 'q';

my $snake = [{ x => 3, y => 3 }];
my $fruit = { x => int(rand($width)), y => int(rand($height)) };
my $direction = 'right';
my $score = 0;
my $running = 1;

ReadMode('cbreak');
END { ReadMode('normal') }

# ---------- main execution code ----------

while ($running) {
    system("clear");
    my @board = ([' '] x $width) x $height;
    $board[$fruit->{y}][$fruit->{x}] = $fruit_char;
    foreach my $part (@$snake) {
        $board[$part->{y}][$part->{x}] = $snake_char;
    }
    foreach my $row (@board) {
        print join("", @$row), "\n";
    }
    print "Score: $score\n";
    if (defined (my $key = ReadKey(-1))) {
        $direction = 'up' if $key eq 'w';
        $direction = 'down' if $key eq 's';
        $direction = 'left' if $key eq 'a';
        $direction = 'right' if $key eq 'd';
        $running = 0 if $key eq $quit_key;
    }
    my $head = $snake->[0];
    my $new_head = { %$head };
    if ($direction eq 'up') {
        $new_head->{y}--;
    } elsif ($direction eq 'down') {
        $new_head->{y}++;
    } elsif ($direction eq 'left') {
        $new_head->{x}--;
    } elsif ($direction eq 'right') {
        $new_head->{x}++;
    }
    if ($new_head->{x} < 0 || $new_head->{x} >= $width ||
        $new_head->{y} < 0 || $new_head->{y} >= $height ||
        grep { $_->{x} == $new_head->{x} && $_->{y} == $new_head->{y} } @$snake) {
        $running = 0;
    }
    if ($new_head->{x} == $fruit->{x} && $new_head->{y} == $fruit->{y}) {
        $score++;
        $fruit = { x => int(rand($width)), y => int(rand($height)) };
    } else {
        pop @$snake;
    }
    unshift @$snake, $new_head;
    select(undef, undef, undef, $snake_speed);
}
print "\nGame Over! Your score: $score\n";