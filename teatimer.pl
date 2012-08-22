#!/usr/bin/perl

use warnings;
use strict;

use LWP::Simple;
use Win32::SerialPort;
use IO::Handle;
#use Data::Dumper;

#where to send data
my $url = "http://example.com/wth.php?v=";

my $serial_port = new Win32::SerialPort('COM4');

$serial_port->handshake('none');
$serial_port->baudrate(9600);
$serial_port->parity('odd');
$serial_port->databits(7);
$serial_port->stopbits(1);
$serial_port->buffers(256, 256);
$serial_port->read_interval(0); #RI
$serial_port->read_const_time(20); #RC

open my($fd), ">>" , 'wth3.txt' || die('cant append to file');

my $data = "";
my $response = $serial_port->input;
while (1) { 
    my ($rb, $byte) = $serial_port->read(1);
    if ($rb > 0) {
        #print $byte;
        $data = "$data$byte";
        if($byte eq "\n") {
            $data =~ s/^[^,]+//;
            $data = time().$data;
            chomp($data);
            $url = $url.$data;
            print "$data\n";
            print $fd "$data\n";
            $fd->flush;
            $data = "";
            my $contents = get($url);
        }
    }
}

#print Dumper($response);
