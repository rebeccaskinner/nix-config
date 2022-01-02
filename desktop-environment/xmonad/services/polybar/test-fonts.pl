#! /usr/bin/env nix-shell
#! nix-shell -i perl -p perl534 perl534Packages.FontTTF
#testFon-fonts.pl
use strict;
use warnings;
use Font::FreeType;
print "looking for characters..."
# my ($char) = @ARGV;
# foreach my $font_def (`fc-list`) {
#     my ($file, $name) = split(/: /, $font_def);
#     my $face = Font::FreeType->new->face($file);
#     my $glyph = $face->glyph_from_char($char);
#     if ($glyph) {
#         print $font_def;
#     }
# }
