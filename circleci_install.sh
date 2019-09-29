#!/bin/bash -e

apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential bzip2 ca-certificates perl
PERL_MM_USE_DEFAULT=1 cpan Perl::Tidy
