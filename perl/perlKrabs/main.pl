#!/usr/bin/perl
use strict;
use warnings;
use LWP::UserAgent;
use URI::Escape;
if (@ARGV != 1) {
    die "Usage: $0 <search_query>\n";
}
my $query = $ARGV[0];
my $encoded_query = uri_escape($query);
my $url = "https://www.google.com/search?q=$encoded_query";
my $ua = LWP::UserAgent->new;
my $response = $ua->get($url);
if ($response->is_success) {
    print $response->decoded_content;
} else {
    die $response->status_line;
}