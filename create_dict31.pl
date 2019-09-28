use strict;
use warnings;
use feature 'switch';
use locale;
use Unicode::UCD 'charinfo';
use Encode 'decode_utf8';
use Data::Dump qw(dump);
use Getopt::Long;
binmode(STDOUT, ":utf8");

# LOAD DICTIONARY FILE INTO A MULTIDIMENSIONAL ASSOCIATIVE ARRAY
sub load_dictionary {
    my ($dict_path) = @_;
    my @words = ();
    open(DICT, "<:utf8:crlf", $dict_path) or die "Cannot open file: $dict_path";
    foreach my $line (<DICT>) {
        #SOME NORMALIZATION: all will be now in lowercase, eth will be thorns and first vowel in dipht. will be accented
        $line = move_accents(eth2thorn(lc($line)));
        my @splitarray = split(/\t/, $line);
        my $mypp       = 0;
        my $mypspa     = 0;
        if ($splitarray[7] eq "1") {
            if   ($splitarray[1] =~ m/nde$/) { $mypspa = 1; }
            else                             { $mypp   = 1; }
        }    #If the form is a participle, decide, whether it is pspa or ppa
        my %assoc_array = (
            "nid",          $splitarray[0],  "title",         $splitarray[1],
            "wright",       $splitarray[2],  "noun",          $splitarray[3],
            "pronoun",      $splitarray[4],  "adjective",     $splitarray[5],
            "verb",         $splitarray[6],  "participle",    $splitarray[7],
            "pspart",       $mypspa,         "papart",        $mypp,
            "adverb",       $splitarray[8],  "preposition",   $splitarray[9],
            "conjunction",  $splitarray[10], "interjection",  $splitarray[11],
            "numeral",      $splitarray[12], "vb_weak",       $splitarray[13],
            "vb_strong",    $splitarray[14], "vb_contracted", $splitarray[15],
            "vb_pretpres",  $splitarray[16], "vb_anomalous",  $splitarray[17],
            "vb_uncertain", $splitarray[18], "n_masc",        $splitarray[19],
            "n_fem",        $splitarray[20], "n_neut",        $splitarray[21],
            "n_uncert", $splitarray[22], "vb_paradigm", [],
            "adj_paradigm", [], "noun_paradigm", [],
            "syllables", "", "prefix", "",
            "long_stem", "", "stem",   $splitarray[1]
        );
        push(@words, \%assoc_array);
    }
    close(DICT);
    return @words;
}

# LOAD MANUAL FORMS INTO THE FORMS ARRAY
sub print_manual_forms {
    my ($manual_forms_path) = @_;
    open(MANFORMS, "<:utf8:crlf", $manual_forms_path) or die "Cannot open file: $manual_forms_path";
    foreach my $forms_line (<MANFORMS>) {
        $forms_line =~ s/\n//;
        $forms_line = move_accents(eth2thorn($forms_line));
        my @splitarray = split(/\t/, $forms_line);
        my %form = ();
        $form{BT}    = $splitarray[0];
        $form{title} = lc($splitarray[1]);
        $form{stem}  = lc($splitarray[1]);
        $form{stem} =~ s/.*-//;
        $form{form} = lc($splitarray[3]);
        $form{form} =~ s/-//;
        $form{formParts}   = lc($splitarray[3]);
        $form{var}         = $splitarray[5];
        $form{probability} = $splitarray[6];
        $form{function}    = $splitarray[7];
        $form{wright}      = $splitarray[8];
        $form{paradigm}    = lc($splitarray[9]);
        $form{paraID}      = $splitarray[10];
        $form{wordclass}   = lc($splitarray[11]);
        $form{class1}      = lc($splitarray[12]);
        $form{class2}      = lc($splitarray[13]);
        $form{class3}      = lc($splitarray[14]);
        $form{comment}     = $splitarray[15];
    }
    close(MANFORMS);
}

# LOAD PARADIGMS INTO ASSOCIATIVE ARRAY
sub load_paradigms {
    my ($vp_path) = @_;
    open(VP, "<:utf8:crlf", $vp_path) or die "Cannot open file: $vp_path";
    my @vparadigms;
    foreach my $vparadigm_line (<VP>) {
        $vparadigm_line = eth2thorn(lc($vparadigm_line));
        my @splitarray = split(/\t/, $vparadigm_line);
        my $ID         = $splitarray[0];
        my $variant    = $splitarray[7];
        my $paradigm   = $splitarray[8];
        $splitarray[15] =~ s/\n//;

        unless ($vparadigms[$ID]) {
            %{ $vparadigms[$ID] } = (
                "ID",          $splitarray[0], "title",    eth2thorn($splitarray[1]),
                "type",        $splitarray[2], "class",    $splitarray[3],
                "subdivision", $splitarray[4], "subclass", $splitarray[5],
                "wright",      $splitarray[6]
            );
        }
        unless ($vparadigms[$ID]{variant}[$variant]) {
            %{ $vparadigms[$ID]{variant}[$variant] } = ("variantID", $variant);
        }
        %{ $vparadigms[$ID]{variant}[$variant]{$paradigm} } = (
            "paraID", $paradigm,       "prefix",    $splitarray[9],  "preVowel", $splitarray[10],
            "vowel",  $splitarray[11], "postVowel", $splitarray[12], "boundary", $splitarray[13],
            "dental", $splitarray[14], "ending",    $splitarray[15]
        );
    }
    close VP;
    return \@vparadigms;
}

#- CONSTANTS AND GENERAL SUBROUTINES ---------------------------------------------------------------------------------------------------------------------------------
#CONSTANTS
sub set_constants {
    my ($prefix_path) = @_;
    use constant VOWEL =>
"[\x{00E6}aeiyou\x{0153}\x{00C6}AEIYOU\x{0152}\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}]";
    $vowel_r =
"[\x{00E6}aeiyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}]";
    $vowel_regex = qr/${\(VOWEL)}/;
    use constant LVOWEL =>
"[\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}]";
    $lvowel_regex = qr/${\(LVOWEL)}/;
    use constant DIPHTHONG => "([Ee][AaOo])|([Ii][Ee])";
    $diphthong_regex = qr/${\(DIPHTHONG)}/;
    use constant LDIPHTHONG => "([\x{00C9}\x{00E9}][AaOo])|([\x{00CD}\x{00ED}][Ee])";
    $ldiphthong_regex = qr/${\(LDIPHTHONG)}/;
    use constant CONSONANT =>
"[^\x{00E6}aeiyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}]";
    $consonant_regex = qr/${\(CONSONANT)}/;

    $prefix_regex    = "0";
    open(PFX, "<:utf8:crlf", $prefix_path) or die "Cannot open file: $prefix_path";
    foreach (<PFX>) { $prefix_regex = "$prefix_regex|$_"; }
    close PFX;
    $prefix_regex =~ s/\n//g;
    $prefix_regex =~ s/0\|//g;
}

# REMOVE PREFIXES (OR ANYTHING BEFORE THE LAST??? HYPHEN/space) AND STORE IT IN PREFIX
sub remove_prefix {
    my @mywords = @_;
    for my $i (0 .. $#mywords) {
        $mywords[$i]{prefix} = "0";
        if ($mywords[$i]{stem} =~ s/(.*)[\- ]//) { $mywords[$i]{prefix} = $1; }
    }
    return @mywords;
}

# REMOVE HYPHENS
sub remove_hyphens {
    my @mywords = @_;
    for my $i (0 .. $#mywords) {
        $mywords[$i]{prefix} =~ s/-//g;    # IN CASE OF MORE HYPHENS
        $mywords[$i]{stem}   =~ s/-//g;

    }
    return @mywords;
}

# ETH TO THORN
sub eth2thorn {
    my $mywords = $_[0];
    $mywords =~ s/[\x{00F0}]/\x{00FE}/g;
    $mywords =~ s/[\x{00D0}]/\x{00DE}/g;
    $mywords =~ s/k/c/ig;
    return $mywords;
}

# REMOVE DIACRITICS
sub remove_dia {
    my $mywords = $_[0];
    $mywords =~ s/\x{01FD}/\x{00E6}/g;
    $mywords =~ s/\x{00E1}/a/g;
    $mywords =~ s/\x{00E9}/e/g;
    $mywords =~ s/\x{00ED}/i/g;
    $mywords =~ s/\x{00FD}/y/g;
    $mywords =~ s/\x{00F3}/o/g;
    $mywords =~ s/\x{00FA}/u/g;
    return $mywords;
}

# I-UMLAUT
sub iumlaut {
    my @myvowels = @_;

    # this can not be a historically accurate iumlaut, since we are working synchron. from infinitive
    $myvowels[1] = $myvowels[0];
    $myvowels[0] =~ s/^e$/i/;           # e > i
    $myvowels[0] =~ s/^o$/e/;           # o > e
    $myvowels[0] =~ s/^u$/y/;           # u > y
    $myvowels[0] =~ s/^\x{00E6}$/e/;    # æ > e
    if ($myvowels[0] =~ s/^a$/\x{00E6}/) { $myvowels[2] = "e"; }    # a > æ (later e)
    $myvowels[0] =~ s/^\x{00E1}$/\x{001FD}/;                        # á > ǽ
    $myvowels[0] =~ s/^\x{00F3}$/\x{00E9}/;                         # ó > é
    $myvowels[0] =~ s/^\x{00FA}$/\x{00FD}/;                         # ú > ý
    if ($myvowels[1] =~ s/^ea$/ie/) { $myvowels[2] = "i"; }         # ea > ie (later > i,y)
    $myvowels[0] =~ s/^eo$/ie/;    # eo > ie (does it happen? C gives > io, but weorpan > wierpst)
    if ($myvowels[0] =~ s/^io$/ie/) { $myvowels[2] = "i"; }    # io > ie (later > i,y)

    if ($myvowels[0] =~ s/^\x{00E9}a$/\x{00ED}e/) {
        $myvowels[2] = "\x{00ED}";
    }                                                          # éa > íe (later > í,ý but sometimes short ie?)
    $myvowels[0] =~ s/^\x{00E9}o$/\x{00ED}e/;                                         # éo > íe (does it happen?)
    if ($myvowels[0] =~ s/^[\x{00ED}]o$/\x{00ED}e/) { $myvowels[2] = "\x{00ED}"; }    # ío > íe (later > í,ý)

    # we return an array of vowels in mutated and non-mutated forms, 1 is unmutated
    return @myvowels;
}

# MOVE ACCENTS FOR BT DIPHTHONGS
sub move_accents {
    my $mywords = $_[0];
    $mywords =~ s/e\x{00F3}/\x{00E9}o/g;                                              #eó > éo
    $mywords =~ s/e\x{00E1}/\x{00E9}a/g;                                              #eá > éa
    $mywords =~ s/i\x{00E9}/\x{00ED}e/g;                                              #ié > íe
    return $mywords;
}

# CALCULATE STEM LENGTH
sub stem_length {
    my ($mystem) = @_;
    my $mylength = 0;
    $mystem =~
m/^.*?($vowel_regex$vowel_regex?)([^\x{00E6}aeiyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}]*)(.*)/;
    my $myvowel      = $1;
    my $myconsonants = "";
    my $mysecond     = "";
    if ($3) { $mysecond = $3; }
    if ($2) { $myconsonants = $2; $myconsonants =~ s/sc/s/; $myconsonants =~ s/cg/c/; }
    if ($myvowel =~ m/$lvowel_regex/) { $mylength = 1; }  #long stem vowel = long stem
    if (length($myconsonants) > 1)    { $mylength = 1; }  #two or more consonants = stem ends in a consonant = long stem

    if (($mysecond eq "") && (length($myconsonants) > 0)) {
        $mylength = 1;
    }                                                     #monosylabic stem ending in a consonant = long stem
    return $mylength;
}

# COUNT SYLLABLES
sub count_syllables {
    my @mywords = @_;
    my $counter = 0;
    for my $i (0 .. $#mywords) {
        $counter = ($mywords[$i]{stem} =~ s/($vowel_regex$consonant_regex)/$1/g)
          ;                                               #how many times vowel+consonant combination appears in stem
        $counter = $counter +
          ($mywords[$i]{stem} =~ s/($vowel_regex)$/$1/g);    #plus the final vowel, if there is one = syllable count
        if ($mywords[$i]{stem} !~ m/$consonant_regex/) { $counter = 1; }   #if there is no consonant, syllable count = 1
        $mywords[$i]{syllables} = $counter;
    }
    return @mywords;
}

# COUNT SYLLABLES for a single item
sub syllab {
    my $myword  = @_[0];
    my $counter = 0;
    $counter =
      ($myword =~ s/($vowel_regex$consonant_regex)/$1/g);    #how many times vowel+consonant combination appears in stem
    $counter = $counter + ($myword =~ s/($vowel_regex)$/$1/g);   #plus the final vowel, if there is one = syllable count
    return $counter;
}

#-- SETTING PARADIGMS PER WORDCLASSES-------------------------------------------------------------------------------------------------------
# SET PARADIGMS FOR VERBS
sub set_verb_paradigm {
    my ($wordsref, $vparadigms) = @_;
    my @mywords = @{$wordsref};
    my @assigned_mywords;
    my @verbs_mywords;
    my @unassigned_verbs_mywords;
    my $num_verbs;

    for my $i (0 .. $#mywords) {
        if (($mywords[$i]{verb} == 1) && ($mywords[$i]{pspart} + $mywords[$i]{papart} == 0)) {
            push(@verbs_mywords, $mywords[$i]);
        }
    }    #all verbs that are not participles
    $num_verbs = scalar(@verbs_mywords);

    # FIRST THOSE THAT ARE PARADIGM EXAMPLES THEMSELVES (BY STEM COMPARISON AND RESTRICTED BY SAME CLASS MEMBERSHIP)
    for my $i (0 .. $#verbs_mywords) {
        for (my $k1 = 0 ; $k1 < @vparadigms ; $k1++) {
            my $vparadigm = $vparadigms->[$k1]{title};
            my $vtype     = $vparadigms->[$k1]{type};
            if (
                ($verbs_mywords[$i]{stem} eq $vparadigm)
                && (   (($vtype eq "w") && ($verbs_mywords[$i]{vb_weak} == 1))
                    || (($vtype eq "pp") && ($verbs_mywords[$i]{vb_pretpres} == 1))
                    || (($vtype eq "a")  && ($verbs_mywords[$i]{vb_anomalous} == 1))
                    || (($vtype eq "s")  && ($verbs_mywords[$i]{vb_strong} == 1)))
              )
            {
                push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$k1]);
            }
        }
        if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
    }
    print STDERR scalar(@assigned_mywords) . " verbs assigned as paradigm examples.\n";

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # THEN ALL THOSE UNASSIGNED THAT HAVE A STEM SIMILAR TO ASSIGNED VERBS (COMPARING ONLY THOSE OF THE SAME CLASS)
    my $maxy = $#assigned_mywords;
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            my $y = 0;
            while ($y < $maxy) {
                if (
                       ($verbs_mywords[$i]{stem} eq $assigned_mywords[$y]{stem})
                    && ($assigned_mywords[$y]{vb_paradigm}[0])
                    && (   ($verbs_mywords[$i]{vb_strong} == $assigned_mywords[$y]{vb_strong})
                        || ($verbs_mywords[$i]{vb_weak} == $assigned_mywords[$y]{vb_weak})
                        || ($verbs_mywords[$i]{vb_pretpres} == $assigned_mywords[$y]{vb_pretpres})
                        || ($verbs_mywords[$i]{vb_anomalous} == $assigned_mywords[$y]{vb_anomalous})
                        || ($verbs_mywords[$i]{vb_uncertain} == 1))
                  )
                {
                    unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
                        @{ $verbs_mywords[$i]{vb_paradigm} } = @{ $assigned_mywords[$y]{vb_paradigm} };
                    }
                }
                $y++;
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # LET'S DO IT AGAIN, BUT WE ALSO TEST FOR A SECOND PREFIX IN STEM AND WE IGNORE I/Y
    my $maxy = $#assigned_mywords;
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            my $y          = 0;
            my $mod_match1 = $verbs_mywords[$i]{stem};
            $mod_match1 =~ s/^($prefix_regex)-?(.*)/$2/g;
            my $mod_match2 = $mod_match1;
            $mod_match2 =~ s/y/i/g;
            my $mod_match3 = $mod_match1;
            $mod_match3 =~ s/i/y/g;
            my $mod_match4 = $mod_match2;
            $mod_match4 =~ s/i/ie/g;

            while ($y < $maxy) {
                if (
                    (
                           ($mod_match1 eq $assigned_mywords[$y]{stem})
                        || ($mod_match2 eq $assigned_mywords[$y]{stem})
                        || ($mod_match3 eq $assigned_mywords[$y]{stem})
                        || ($mod_match4 eq $assigned_mywords[$y]{stem})
                    )
                    && (   ($verbs_mywords[$i]{vb_strong} == $assigned_mywords[$y]{vb_strong})
                        || ($verbs_mywords[$i]{vb_weak} == $assigned_mywords[$y]{vb_weak})
                        || ($verbs_mywords[$i]{vb_pretpres} == $assigned_mywords[$y]{vb_pretpres})
                        || ($verbs_mywords[$i]{vb_anomalous} == $assigned_mywords[$y]{vb_anomalous})
                        || ($verbs_mywords[$i]{vb_uncertain} == 1))
                  )
                {
                    unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
                        @{ $verbs_mywords[$i]{vb_paradigm} } = @{ $assigned_mywords[$y]{vb_paradigm} };

                        #since we have discovered an unmarked second prefix, we store a new, shorter stem
                        $verbs_mywords[$i]{stem} = $mod_match1;
                    }
                }
                $y++;
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }
    print scalar(@assigned_mywords) . " verbs verbs assigned as examples and by stem comparison.\n";

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # THEN ALL THOSE KNOWN FROM WRIGHT BY PARAGRAPHS
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {    #unless it's already assigned
            for (my $k1 = 0 ; $k1 < @vparadigms ; $k1++) {
                my $wrightparagraphs = $vparadigms[$k1]{wright};
                if (($verbs_mywords[$i]{wright} =~ m/$wrightparagraphs/) && ($wrightparagraphs != 0)) {
                    while ($verbs_mywords[$i]{wright} =~ m/$wrightparagraphs/g) {
                        push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms[$k1]);
                    }
                }
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }
    print scalar(@assigned_mywords) . " verbs assigned as paradigm examples, by stem comparison and by Wright\n";

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

   # THEN AGAIN ALL THOSE UNASSIGNED THAT HAVE A STEM SIMILAR TO ASSIGNED VERBS (COMPARING ONLY THOSE OF THE SAME CLASS)
    my $maxy = $#assigned_mywords;
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            my $y = 0;
            while ($y < $maxy) {
                if (
                       ($verbs_mywords[$i]{stem} eq $assigned_mywords[$y]{stem})
                    && ($assigned_mywords[$y]{vb_paradigm}[0])
                    && (   ($verbs_mywords[$i]{vb_strong} == $assigned_mywords[$y]{vb_strong})
                        || ($verbs_mywords[$i]{vb_weak} == $assigned_mywords[$y]{vb_weak})
                        || ($verbs_mywords[$i]{vb_pretpres} == $assigned_mywords[$y]{vb_pretpres})
                        || ($verbs_mywords[$i]{vb_anomalous} == $assigned_mywords[$y]{vb_anomalous})
                        || ($verbs_mywords[$i]{vb_uncertain} == 1))
                  )
                {
                    unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
                        @{ $verbs_mywords[$i]{vb_paradigm} } = @{ $assigned_mywords[$y]{vb_paradigm} };
                    }
                }
                $y++;
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # LET'S DO IT ONCE AGAIN, BUT WE ALSO TEST FOR A SECOND PREFIX IN STEM AND WE IGNORE I/Y
    my $maxy = $#assigned_mywords;
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            my $y          = 0;
            my $mod_match1 = $verbs_mywords[$i]{stem};
            $mod_match1 =~ s/^($prefix_regex)-?(.*)/$2/g;
            my $mod_match2 = $mod_match1;
            $mod_match2 =~ s/y/i/g;
            my $mod_match3 = $mod_match1;
            $mod_match3 =~ s/i/y/g;
            my $mod_match4 = $mod_match2;
            $mod_match4 =~ s/i/ie/g;

            while ($y < $maxy) {
                if (
                    (
                           ($mod_match1 eq $assigned_mywords[$y]{stem})
                        || ($mod_match2 eq $assigned_mywords[$y]{stem})
                        || ($mod_match3 eq $assigned_mywords[$y]{stem})
                        || ($mod_match4 eq $assigned_mywords[$y]{stem})
                    )
                    && (   ($verbs_mywords[$i]{vb_strong} == $assigned_mywords[$y]{vb_strong})
                        || ($verbs_mywords[$i]{vb_weak} == $assigned_mywords[$y]{vb_weak})
                        || ($verbs_mywords[$i]{vb_pretpres} == $assigned_mywords[$y]{vb_pretpres})
                        || ($verbs_mywords[$i]{vb_anomalous} == $assigned_mywords[$y]{vb_anomalous})
                        || ($verbs_mywords[$i]{vb_uncertain} == 1))
                  )
                {
                    unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
                        @{ $verbs_mywords[$i]{vb_paradigm} } = @{ $assigned_mywords[$y]{vb_paradigm} };
                        #since we have discovered an unmarked second prefix, we store a new, shorter stem
                        $assigned_mywords[$y]{stem} = $mod_match1;
                    }
                }
                $y++;
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }
    print scalar(@assigned_mywords) . " verbs verbs assigned as examples, by Wright and by stem comparison.\n";

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # NOW WE DO SOME HEURISTIC TO ASSIGN THE REST - based on Mitchell
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {

            # STRONG VERBS
            if ($verbs_mywords[$i]{vb_strong} == 1) {
                $verbs_mywords[$i]{stem} =~ m/^($vowel_regex*?.*?)($vowel_regex$vowel_regex*)/;
                my $pre_vowel = $1;
                my $vowel     = $2;
                $verbs_mywords[$i]{stem} =~
                  m/^$vowel_regex*?.*?$vowel_regex$vowel_regex*?($consonant_regex.*?)$vowel_regex/;
                my $post_vowel = $1;
                my $post_vowel_length = length($post_vowel);
                my $assigned_paradigm = 0;

                # í/ý + one cons. = bídan
                if ((($vowel eq "\x{00ED}") || ($vowel eq "\x{00FD}")) && ($post_vowel_length == 1)) {
                    $assigned_paradigm = 1;

                    # í/ý+ þ = sníþan
                    if ($post_vowel =~ m/^\x{00FE}/) { $assigned_paradigm = 2; }

                    # í/ý + s = rísan
                    if ($post_vowel =~ m/^s/) { $assigned_paradigm = 3; }
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # éo + one cons. = béodan
                elsif (($vowel eq "\x{00E9}o") && ($post_vowel_length == 1)) {
                    $assigned_paradigm = 5;

                    # éo + s = céosan
                    if ($post_vowel =~ m/^s/) { $assigned_paradigm = 6; }
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # ú + one cons. = brúcan
                elsif (($vowel eq "\x{00FA}") && ($post_vowel_length == 1)) {
                    $assigned_paradigm = 9;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # e + l + one cons. = helpan
                elsif (($vowel eq "e") && ($post_vowel =~ m/^l./)) {
                    $assigned_paradigm = 13;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # e + two cons. = bregdan
                elsif (($vowel eq "e") && ($post_vowel_length == 2)) {
                    $assigned_paradigm = 16;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # e + l/r = beran
                elsif (($vowel eq "e") && ($post_vowel =~ m/^[rl]$/)) {
                    $assigned_paradigm = 19;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # e + [ptcdgfsþ] = metan
                elsif (($vowel eq "e") && ($post_vowel =~ m/^[ptcdgfs\x{00FE}]$/)) {
                    $assigned_paradigm = 22;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # a + one cons. = faran
                elsif (($vowel eq "a") && ($post_vowel_length == 1)) {
                    $assigned_paradigm = 31;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # eo + r/h + one cons. = weorpan/feohtan
                elsif (($vowel eq "eo") && ($post_vowel =~ m/^[rh]./)) {
                    $assigned_paradigm = 14;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # g + ie + two cons. = gieldan
                elsif (($pre_vowel eq "g") && ($vowel eq "ie") && ($post_vowel_length == 2)) {
                    $assigned_paradigm = 63;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # i/y + nasal + one cons. = bindan
                elsif ((($vowel eq "i") || ($vowel eq "y") || ($vowel eq "ie")) && ($post_vowel =~ m/[nm]./)) {
                    $assigned_paradigm = 10;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # á = hátan
                elsif (($vowel eq "\x{00E1}") && ($post_vowel_length == 1)) {
                    $assigned_paradigm = 40;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # ó = blótan
                elsif (($vowel eq "\x{00F3}") && ($post_vowel_length == 1)) {
                    $assigned_paradigm = 48;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }
            }

            # WEAK VERBS

            if ($verbs_mywords[$i]{vb_weak} == 1) {
                $verbs_mywords[$i]{stem} =~ m/^($vowel_regex*?.*?)($vowel_regex$vowel_regex*)/;
                my $pre_vowel = $1;
                my $vowel     = $2;
                $verbs_mywords[$i]{stem} =~
                  m/^$vowel_regex*?.*?$vowel_regex$vowel_regex*?($consonant_regex.*?)$vowel_regex/;
                my $post_vowel = $1;
                my $post_vowel_length = length($post_vowel);
                my $assigned_paradigm = 0;

                # -rian = nerian (except for swarian, gadrian and timbrian)
                if ($verbs_mywords[$i]{stem} =~ m/(rian)|(rigan)|(rgan)$/) {
                    $assigned_paradigm = 73;
                    if (   ($verbs_mywords[$i]{stem} =~ m/swarian$/)
                        || ($verbs_mywords[$i]{stem} =~ m/gadrian$/)
                        || ($verbs_mywords[$i]{stem} =~ m/timbrian$/))
                    {
                        $assigned_paradigm = 87;
                    }
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }

                # the rest of -ian = sealfian
                elsif (($verbs_mywords[$i]{stem} =~ m/($vowel_regex)($consonant_regex*)ian$/)
                    || ($verbs_mywords[$i]{stem} =~ m/($vowel_regex)($consonant_regex*)igan$/))
                {
                    $assigned_paradigm = 87;
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);

                }

                # short vowel + doubled consonant = fremman (except for fyllan)
                elsif (($vowel =~ m/[\x{006C}aeiyou]/) && ($post_vowel =~ m/($consonant_regex)\1/)) {
                    $assigned_paradigm = 74;
                    if (($verbs_mywords[$i]{stem} =~ m/fyllan$/) || ($verbs_mywords[$i]{stem} =~ m/fillan$/)) {
                        $assigned_paradigm = 76;
                    }
                    push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
                }
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }
    print STDERR scalar(@assigned_mywords) . " verbs assigned as examples, by Wright, by stem comparison and by heuristics.\n";

    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # COMPARE STEMS WITH DIACRITICS DISREGARDED
    my $maxy = $#assigned_mywords;
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            my $y = 0;
            $stem_dia = $verbs_mywords[$i]{stem};
            $stem_dia = remove_dia($stem_dia);
            while ($y < $maxy) {
                $stem_dia_assigned = $assigned_mywords[$y]{stem};
                $stem_dia_assigned = remove_dia($stem_dia_assigned);

                if ($stem_dia eq $stem_dia_assigned) {
                    unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
                        @{ $verbs_mywords[$i]{vb_paradigm} } = @{ $assigned_mywords[$y]{vb_paradigm} };
                    }
                }
                $y++;
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }

    # COMPARE STEMS WITH DIACRITICS DISREGARDED FURTHER PREFIX REMOVED AND i/y/ie equivalency
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    my $maxy = $#assigned_mywords;
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            my $y          = 0;
            my $mod_match1 = $verbs_mywords[$i]{stem};
            $mod_match1 =~ s/^($prefix_regex)-?(.*)/$2/g;
            $mod_match1 = remove_dia($mod_match1);
            my $mod_match2 = $mod_match1;
            $mod_match2 =~ s/y/i/g;
            my $mod_match3 = $mod_match1;
            $mod_match3 =~ s/i/y/g;
            my $mod_match4 = $mod_match2;
            $mod_match4 =~ s/i/ie/g;

            while ($y < $maxy) {
                $stem_dia_assigned = $assigned_mywords[$y]{stem};
                $stem_dia_assigned = remove_dia($stem_dia_assigned);
                if (   ($mod_match1 eq $stem_dia_assigned)
                    || ($mod_match2 eq $stem_dia_assigned)
                    || ($mod_match3 eq $stem_dia_assigned)
                    || ($mod_match4 eq $stem_dia_assigned))
                {
                    unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
                        @{ $verbs_mywords[$i]{vb_paradigm} } = @{ $assigned_mywords[$y]{vb_paradigm} };

                        #since we have discovered an unmarked second prefix, we store a new, shorter stem
                        $assigned_mywords[$y]{stem} = $mod_match1;
                    }
                }
                $y++;
            }
            if ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@assigned_mywords, $verbs_mywords[$i]); }
        }
    }
    print STDERR scalar(@assigned_mywords) . " verbs were assigned after the uncertain verbs were stem-compared.\n";
    for my $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) { push(@unassigned_verbs_mywords, $verbs_mywords[$i]) }
    }
    @verbs_mywords = @unassigned_verbs_mywords;
    undef(@unassigned_verbs_mywords);

    # THE REST OF THE STRONG VERBS ARE ASSIGNED TO HELPAN, ALL OTHER TO DÉMAN - mostly variants of already assigned verbs.
    for $i (0 .. $#verbs_mywords) {
        unless ($verbs_mywords[$i]{vb_paradigm}[0]) {
            if   ($verbs_mywords[$i]{vb_strong} == 1) { $assigned_paradigm = 13; }
            else                                      { $assigned_paradigm = 76; }
            push(@{ $verbs_mywords[$i]{vb_paradigm} }, $vparadigms->[$assigned_paradigm]);
            push(@assigned_mywords,                    $verbs_mywords[$i]);
        }
    }
    print STDERR "$num_verbs out of $#assigned_mywords verbs assigned\n";
    return @assigned_mywords;

}

# SET PARADIGMS FOR ADJECTIVES
sub set_adj_paradigm {
    my @mywords = @_;
    my $assignedcount;
    for my $i (0 .. $#mywords) {
        if ($mywords[$i]{stem} =~ m/feald$/) { $mywords[$i]{numeral} = 0; }
        if (($mywords[$i]{adjective} == 1 && ($mywords[$i]{pspart} + $mywords[$i]{papart} + $mywords[$i]{numeral} == 0))
          )
        { #IS IT AN ADJECTIVE? Don't set paradigms for participles, that will be done while generating adj. forms (in case the verbs are assigned later) and don't set paradigms for numerals
            my $counter = 0;
            while ($mywords[$i]{adj_paradigm}[$counter]) { $counter++; }

            # FIRST ALL THOSE KNOWN FROM WRIGHT
            given ($mywords[$i]{wright}) {
                when (/425/) {
                    if ($mywords[$i]{stem} =~ m/(\x{00E6})|(\x{00C6})|(ea)/) {
                        $mywords[$i]{adj_paradigm}[$counter] = "gl\x{00E6}d";
                        $counter++;    #GLAED
                    }
                    else { $mywords[$i]{adj_paradigm}[$counter] = "til"; $counter++; }
                }    #TIL
                when (/426/) { $mywords[$i]{adj_paradigm}[$counter] = "blind";        $counter++; continue; }    # BLIND
                when (/428/) { $mywords[$i]{adj_paradigm}[$counter] = "h\x{00E9}ah";  $counter++; continue; }    # HEAH
                when (/430/) { $mywords[$i]{adj_paradigm}[$counter] = "manig";        $counter++; continue; }    # MANIG
                when (/431/) { $mywords[$i]{adj_paradigm}[$counter] = "h\x{00E1}lig"; $counter++; continue; }    # HALIG
                when (/434/) { $mywords[$i]{adj_paradigm}[$counter] = "wilde";        $counter++; continue; }    # WILDE
                when (/436/) { $mywords[$i]{adj_paradigm}[$counter] = "gearu";        $counter++; continue; }    # GEARU
                when (/437/) { $mywords[$i]{adj_paradigm}[$counter] = "blind";        $counter++; continue; }    # BLIND
            }
            if ($mywords[$i]{stem} =~ m/\x{00FE}weorh$/) { $mywords[$i]{adj_paradigm}[0] = "\x{00FE}weorh"; }
        }
    }

    for my $y (0 .. $#mywords) {
        if ($mywords[$y]{adj_paradigm}[0]) { $assignedcount++; }
    }
    print STDERR "$assignedcount adjectives assigned by Wright.\n";
    $assignedcount = 0;

    # STEM COMPARISON
    for my $i (0 .. $#mywords) {
        if (($mywords[$i]{adjective} == 1 && ($mywords[$i]{pspart} + $mywords[$i]{papart} + $mywords[$i]{numeral} == 0))
          )
        { #IS IT AN ADJECTIVE? Don't set paradigms for participles, that will be done while generating adj. forms (in case the verbs are assigned later) and don't set paradigms for numerals
            my $counter = 0;
            while ($mywords[$i]{adj_paradigm}[$counter]) { $counter++; }
            if ($counter == 0) {
                for my $y (0 .. $#mywords) {

                    if (
                           $mywords[$y]{adj_paradigm}[0]
                        && ($mywords[$y]{stem} eq $mywords[$i]{stem})
                        && (
                            (
                                $mywords[$i]{adjective} == 1
                                && ($mywords[$i]{pspart} + $mywords[$i]{papart} + $mywords[$i]{numeral} == 0)
                            )
                        )
                      )
                    {
                        $mywords[$i]{adj_paradigm}[0] = $mywords[$y]{adj_paradigm}[0];
                    }

                }
            }
        }
    }

    for my $y (0 .. $#mywords) {
        if ($mywords[$y]{adj_paradigm}[0]) { $assignedcount++; }
    }
    print STDERR "$assignedcount adjectives assigned by Wright and stem comparison.\n";
    $assignedcount = 0;

    # NOW HEURISTICS
    for my $i (0 .. $#mywords) {
        #ALLOW MULTIPLICATIVE NUMERALS TO BE ASSIGNED WITH ADJ
        if ($mywords[$i]{stem} =~ m/feald$/) { $mywords[$i]{numeral} = 0; }
        if (($mywords[$i]{adjective} == 1 && ($mywords[$i]{pspart} + $mywords[$i]{papart} + $mywords[$i]{numeral} == 0))
          )
        { #IS IT AN ADJECTIVE? Don't set paradigms for participles, that will be done while generating adj. forms (in case the verbs are assigned later) and don't set paradigms for numerals
            my $counter = 0;
            while ($mywords[$i]{adj_paradigm}[$counter]) { $counter++; }
            if ($counter == 0) {

                # -sum, -isc, -lic decline like til
                if ($mywords[$i]{stem} =~ m/(sum|lic|l\x{00ED}c|isc)$/) {
                    $mywords[$i]{adj_paradigm}[$counter] = "til";
                    $counter++;    #TIL

                    # -cund, -feald, -faest, -leas, -full, -iht  decline like blind
                }
                elsif ($mywords[$i]{stem} =~ m/(cund|feald|f\x{00E6}st|l\x{00E9}as|full|iht)$/) {
                    $mywords[$i]{adj_paradigm}[$counter] = "blind";
                    $counter++;    #BLIND

                    # -ihte, -baere, -ede, -wende like wilde
                }
                elsif ($mywords[$i]{stem} =~ m/(ihte|b\x{01FD}re|ede|wende)$/) {
                    $mywords[$i]{adj_paradigm}[$counter] = "wilde";
                    $counter++;    #WILDE

                    # MONOSYLLABIC?
                }
                elsif ($mywords[$i]{syllables} < 2) {

                    # MONOSYLLABIC SHORT STEM?
                    if (!&stem_length($mywords[$i]{stem})) {
                        $mywords[$i]{stem} =~ m/^.*?($vowel_regex)/;

                        # MONOSYLLABIC SHORT STEM WITH STEM ASH!
                        if ($mywords[$i]{stem} =~ m/(\x{00E6})|(\x{00C6})|(ea)/) {
                            $mywords[$i]{adj_paradigm}[$counter] = "gl\x{00E6}d";
                            $counter++;    #GLAED

                            # MONOSYLLABIC SHORT STEM WITHOUT STEM ASH!
                        }
                        else { $mywords[$i]{adj_paradigm}[$counter] = "til"; $counter++; }    #TIL
                    }
                    else {
                        if ($mywords[$i]{stem} =~ m/($vowel_regex)h$/) {

                            # MONOSYLLABIC LONG STEM ENDING IN -Vh!
                            $mywords[$i]{adj_paradigm}[$counter] = "h\x{00E9}ah";
                            $counter++;                                                       #HEAH
                        }
                        elsif ($mywords[$i]{stem} =~ m/($consonant_regex)h$/) {

                            # MONOSYLLABIC LONG STEM ENDING IN -Ch!
                            $mywords[$i]{adj_paradigm}[$counter] = "\x{00FE}weorh";
                            $counter++;                                                       #THWEORH
                        }
                        else { $mywords[$i]{adj_paradigm}[$counter] = "blind"; $counter++; }    #BLIND
                    }

                    # POLYSYLLABIC!
                }
                else {
                    if (($mywords[$i]{stem} =~ m/u$/) || ($mywords[$i]{stem} =~ m/o$/)) {

                        # POLYSYLLABIC STEM ENDING IN -u/-o
                        $mywords[$i]{adj_paradigm}[$counter] = "gearu";
                        $counter++;                                                             #GEARU
                    }
                    elsif ($mywords[$i]{stem} =~ m/e$/) {

                        # POLYSYLLABIC STEM ENDING IN -e
                        $mywords[$i]{adj_paradigm}[$counter] = "wilde";
                        $counter++;                                                             #WILDE
                    }
                    else {
                        if (&stem_length($mywords[$i]{stem})) {

                            # LONG POLYSYLLABIC STEM
                            $mywords[$i]{adj_paradigm}[$counter] = "h\x{00E1}lig";
                            $counter++;
                        }                                                                       #HALIG

                        # SHORT POLYSYLLABIC STEM
                        else { $mywords[$i]{adj_paradigm}[$counter] = "manig"; $counter++; }    #MANIG
                    }
                }
            }
        }
    }
    for my $y (0 .. $#mywords) {
        if ($mywords[$y]{adj_paradigm}[0]) { $assignedcount++; }
    }
    print STDERR "$assignedcount adjectives assigned by Wright, stem comparison and heuristics.\n";
    $assignedcount = 0;
    return @mywords;
}

# SET PARADIGMS FOR NOUNS
sub set_noun_paradigm {
    my @mywords = @_;
    my @mynouns;
    my @assigned_nouns;
    for my $i (0 .. $#mywords) {
        if ($mywords[$i]{noun} == 1) { push(@mynouns, $mywords[$i]); }
    }
    for my $i (0 .. $#mynouns) {
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }

        # FIRST ALL THOSE KNOWN FROM WRIGHT
        given ($mynouns[$i]{wright}) {
            when (/335|339|387|354|386|337|340|341|352/) {
                $mynouns[$i]{noun_paradigm}[$counter] = "st\x{00E1}n";
                $counter++;
            }    #STÁN
            when (/356/) { $mynouns[$i]{noun_paradigm}[$counter] = "cynn"; $counter++; }                      #CYNN
            when (/343|349|348/) { $mynouns[$i]{noun_paradigm}[$counter] = "word"; $counter++; continue; }    # WORD
            when (/344|350|357|393|358/) { $mynouns[$i]{noun_paradigm}[$counter] = "hof"; $counter++; continue; }  # HOF
            when (/336/) { $mynouns[$i]{noun_paradigm}[$counter] = "d\x{00E6}g"; $counter++; continue; }    # DÆG
            when (/345/) { $mynouns[$i]{noun_paradigm}[$counter] = "f\x{00E6}t"; $counter++; continue; }    # FÆT
            when (/367|368|373|376|390|366|372|370|375|378/) {
                $mynouns[$i]{noun_paradigm}[$counter] = "\x{00E1}r";
                $counter++;
                continue;
            }                                                                                               # ÁR
            when (/383/)      { $mynouns[$i]{noun_paradigm}[$counter] = "strengu";     $counter++; continue; } # STRENGU
            when (/397/)      { $mynouns[$i]{noun_paradigm}[$counter] = "feld";        $counter++; continue; } # FELD
            when (/398/)      { $mynouns[$i]{noun_paradigm}[$counter] = "hand";        $counter++; continue; } # HAND
            when (/396/)      { $mynouns[$i]{noun_paradigm}[$counter] = "sunu";        $counter++; continue; } # SUNU
            when (/398/)      { $mynouns[$i]{noun_paradigm}[$counter] = "duru";        $counter++; continue; } # DURU
            when (/359|360/)  { $mynouns[$i]{noun_paradigm}[$counter] = "bearu";       $counter++; continue; } # BEARU
            when (/362|363/)  { $mynouns[$i]{noun_paradigm}[$counter] = "bealu";       $counter++; continue; } # BEALU
            when (/3]80|381/) { $mynouns[$i]{noun_paradigm}[$counter] = "beadu";       $counter++; continue; } # BEADU
            when (/401/)      { $mynouns[$i]{noun_paradigm}[$counter] = "guma";        $counter++; continue; } # GUMA
            when (/402/)      { $mynouns[$i]{noun_paradigm}[$counter] = "fr\x{00E9}a"; $counter++; continue; } # FRÉA
            when (/404/)      { $mynouns[$i]{noun_paradigm}[$counter] = "tunge";       $counter++; continue; } # TUNGE
            when (/405/)      { $mynouns[$i]{noun_paradigm}[$counter] = "b\x{00E9}o";  $counter++; continue; } # BÉO
            when (/407/)      { $mynouns[$i]{noun_paradigm}[$counter] = "\x{00E9}age"; $counter++; continue; } # ÉAGE
            when (/418/) { $mynouns[$i]{noun_paradigm}[$counter] = "w\x{00ED}gend"; $counter++; continue; }    # WÍGEND
        }
        if ($counter) { push(@assigned_nouns, $mynouns[$i]); }
    }
    print STDERR "$#assigned_nouns nouns assigned by Wright.\n";

    #BY STEM COMPARISON WITH THOSE KNOWN FROM WRIGHT
    my $assignednum = $#assigned_nouns;
    for my $i (0 .. $#mynouns) {
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }
        if ($counter == 0) {
            for my $i2 (0 .. $assignednum) {
                if (   ($mynouns[$i]{stem} eq $assigned_nouns[$i2]{stem})
                    && ($assigned_nouns[$i2]{noun_paradigm}[$counter]))
                {
                    $mynouns[$i]{noun_paradigm}[$counter] = $assigned_nouns[$i2]{noun_paradigm}[$counter];
                    $counter++;
                }
            }
            if ($counter) { push(@assigned_nouns, $mynouns[$i]); }
        }
    }
    print STDERR "$#assigned_nouns nouns assigned by Wright and simple stem comparison.\n";

    #BY ADVANCED STEM COMPARISON WITH THOSE KNOWN FROM WRIGHT
    my $assignednum = $#assigned_nouns;
    for my $i (0 .. $#mynouns) {
        my $mod_match1 = $mynouns[$i]{stem};
        $mod_match1 =~ s/^($prefix_regex)-?(.*)/$2/g;
        my $mod_match2 = $mod_match1;
        $mod_match2 =~ s/y/i/g;
        my $mod_match3 = $mod_match1;
        $mod_match3 =~ s/i/y/g;
        my $mod_match4 = $mod_match2;
        $mod_match4 =~ s/i/ie/g;
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }

        if ($counter == 0) {
            for my $i2 (0 .. $assignednum) {
                if (
                    (
                           ($mod_match1 eq $assigned_nouns[$i2]{stem})
                        || ($mod_match2 eq $assigned_nouns[$i2]{stem})
                        || ($mod_match3 eq $assigned_nouns[$i2]{stem})
                        || ($mod_match4 eq $assigned_nouns[$i2]{stem})
                    )
                    && ($assigned_nouns[$i2]{noun_paradigm}[$counter])
                  )
                {
                    $mynouns[$i]{noun_paradigm}[$counter] = $assigned_nouns[$i2]{noun_paradigm}[$counter];
                    $counter++;
                }
            }
            if ($counter) { push(@assigned_nouns, $mynouns[$i]); }
        }
    }
    print STDERR "$#assigned_nouns nouns assigned by Wright and advanced stem comparison.\n";

    for my $i (0 .. $#mynouns) {
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }

        # NOW THOSE UNKNOWN TO WRIGHT - might be a good idea to detect a second prefix as with verbs?
        if ($counter == 0) {
            $mynouns[$i]{stem} =~ m/^($vowel_regex?$vowel_regex?.*?)($vowel_regex$vowel_regex?)/;
            my $vowel = $2;

            #HEURISTICS
            # -a > weak
            if ($mynouns[$i]{stem} =~ m/a$/) {

                #a. –a & short V > guma (W401)
                if ($vowel =~ m/$lvowel_regex/) {
                    $mynouns[$i]{noun_paradigm}[$counter] = "fr\x{00E9}a";
                    $counter++;
                    #b. –a & long V > fréa (W402)
                }
                else { $mynouns[$i]{noun_paradigm}[$counter] = "guma"; $counter++; }
            }
            if ($mynouns[$i]{stem} =~ m/e$/) {

                #c. –e & fem. > tunge (W404)
                if ($mynouns[$i]{n_fem} == 1) { $mynouns[$i]{noun_paradigm}[$counter] = "tunge"; $counter++; }

                #d. –e & masc. > ja/i stems stán (wine W386 = ende W354)
                if ($mynouns[$i]{n_masc} == 1) { $mynouns[$i]{noun_paradigm}[$counter] = "st\x{00E1}n"; $counter++; }

                #e. –e & neut. > ja/i stems hof (spere W393 = wíte W357)
                if ($mynouns[$i]{n_neut} == 1) { $mynouns[$i]{noun_paradigm}[$counter] = "hof"; $counter++; }
            }

            #f. –nd & masc. > wígend (W418)
            if (($mynouns[$i]{stem} =~ m/nd$/) && ($mynouns[$i]{n_masc} == 1)) {
                $mynouns[$i]{noun_paradigm}[$counter] = "w\x{00ED}gend";
                $counter++;
            }

            #g. –els, –scipe > stán
            if ($mynouns[$i]{stem} =~ m/(els)|(scipe)$/) {
                $mynouns[$i]{noun_paradigm}[$counter] = "st\x{00E1}n";
                $counter++;
            }

            #h. –incel > hof
            if ($mynouns[$i]{stem} =~ m/incel$/) { $mynouns[$i]{noun_paradigm}[$counter] = "hof"; $counter++; }

            #i. –ness, -niss, -ung > ár
            if ($mynouns[$i]{stem} =~ m/(ness)|(niss)|(nyss)|(ung)$/) {
                $mynouns[$i]{noun_paradigm}[$counter] = "\x{00E1}r";
                $counter++;
            }

            #g. –mono syl. & root V & masculine = æ/ǽ > dæg (W336, i-mutation!)
            if (($vowel =~ m/\x{00E6}|\x{01FD}/) && ($mywords[$i]{syllables} < 2)) {
                if ($mynouns[$i]{n_masc} == 1) { $mynouns[$i]{noun_paradigm}[$counter] = "d\x{00E6}g"; $counter++; }

                #h. –mono syl. & root V & neuter = æ/ǽ > fæt (W345, i-mutation!)
                if ($mynouns[$i]{n_neut} == 1) { $mynouns[$i]{noun_paradigm}[$counter] = "f\x{00E6}t"; $counter++; }
            }
            if ($counter) { push(@assigned_nouns, $mynouns[$i]); }
        }
    }
    print STDERR "$#assigned_nouns  nouns assigned by Wright,  stem comparison and heuristics.\n";

    #BY STEM COMPARISON WITH THOSE KNOWN FROM WRIGHT
    my $assignednum = $#assigned_nouns;
    for my $i (0 .. $#mynouns) {
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }
        if ($counter == 0) {
            for my $i2 (0 .. $assignednum) {
                if (   ($mynouns[$i]{stem} eq $assigned_nouns[$i2]{stem})
                    && ($assigned_nouns[$i2]{noun_paradigm}[$counter]))
                {
                    $mynouns[$i]{noun_paradigm}[$counter] = $assigned_nouns[$i2]{noun_paradigm}[$counter];
                    $counter++;
                }
            }
            if ($counter) { push(@assigned_nouns, $mynouns[$i]); }
        }
    }
    print STDERR "$#assigned_nouns nouns assigned by second stem comparison.\n";

    #BY ADVANCED STEM COMPARISON WITH THOSE KNOWN FROM WRIGHT
    my $assignednum = $#assigned_nouns;
    for my $i (0 .. $#mynouns) {
        my $mod_match1 = $mynouns[$i]{stem};
        $mod_match1 =~ s/^($prefix_regex)-?(.*)/$2/g;
        my $mod_match2 = $mod_match1;
        $mod_match2 =~ s/y/i/g;
        my $mod_match3 = $mod_match1;
        $mod_match3 =~ s/i/y/g;
        my $mod_match4 = $mod_match2;
        $mod_match4 =~ s/i/ie/g;
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }

        if ($counter == 0) {
            for my $i2 (0 .. $assignednum) {
                if (
                    (
                           ($mod_match1 eq $assigned_nouns[$i2]{stem})
                        || ($mod_match2 eq $assigned_nouns[$i2]{stem})
                        || ($mod_match3 eq $assigned_nouns[$i2]{stem})
                        || ($mod_match4 eq $assigned_nouns[$i2]{stem})
                    )
                    && ($assigned_nouns[$i2]{noun_paradigm}[$counter])
                  )
                {
                    $mynouns[$i]{noun_paradigm}[$counter] = $assigned_nouns[$i2]{noun_paradigm}[$counter];
                    $counter++;
                }
            }
            if ($counter) { push(@assigned_nouns, $mynouns[$i]); }
        }
    }
    print STDERR "$#assigned_nouns nouns assigned by second advanced stem comparison.\n";

    #THE REST IS UNKNOWN AND THEREFORE ASSIGNED ACCORDING TO GENDER AND STEM LENGTH
    for my $i (0 .. $#mynouns) {
        my $counter = 0;
        while ($mynouns[$i]{noun_paradigm}[$counter]) { $counter++; }
        if ($counter == 0) {

            #masculine > stán
            if ($mynouns[$i]{n_masc} == 1 || $mynouns[$i]{n_uncert} == 1) {
                $mynouns[$i]{noun_paradigm}[$counter] = "st\x{00E1}n";
                $counter++;
            }

            #neuter long stem > word
            if (($mynouns[$i]{n_neut} == 1) && (&stem_length($mywords[$i]{stem}))) {
                $mynouns[$i]{noun_paradigm}[$counter] = "word";
                $counter++;
            }

            #neuter short stem > hof
            if (($mynouns[$i]{n_neut} == 1) && (!&stem_length($mywords[$i]{stem}))) {
                $mynouns[$i]{noun_paradigm}[$counter] = "hof";
                $counter++;
            }

            #feminine > ár
            if ($mynouns[$i]{n_fem} == 1) { $mynouns[$i]{noun_paradigm}[$counter] = "\x{00E1}r"; $counter++; }

            push(@assigned_nouns, $mynouns[$i]);
        }
    }
    print STDERR "$#assigned_nouns out of $#mynouns nouns assigned.\n";

    return @mynouns;
}

#-- GENERATING FORMS BY WORDCLASS -------------------------------------------------------------------------------------------------------

#GENERATE NOMINAL FORMS
sub generate_nounforms {
    my @mywords = @_;
    print STDERR "Generating nominal forms from $#mywords nouns.\n";
    my %formhash;
    my $form;
    my $form_parts;
    my @stem;
    my $alt_stem;
    for my $i (0 .. $#mywords) {
        my $bt_id = sprintf("%06d", $mywords[$i]{nid});

        for my $i2 (0 .. scalar(@{ $mywords[$i]{noun_paradigm} })) {

            #STRONG
            %formhash = (
                "title", $mywords[$i]{title},
                "stem", $mywords[$i]{stem},
                "BT", $bt_id, "wordclass", "noun", "class1", "strong", "paradigm", $mywords[$i]{noun_paradigm}[$i2],
                "wright", $mywords[$i]{wright}
            );

            # STAN
            if ($mywords[$i]{noun_paradigm}[$i2] =~ m/st\x{00E1}n|cynn/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "SgMaNo";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "SgNeNo";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "SgMaAc";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "SgNeAc";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $stem[0] =~ s/h$//g;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgMaGe";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "SgNeGe";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-es";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $stem[0] =~ s/h$//g;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgMaDa";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "SgNeDa";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $stem[0] =~ s/h$//g;
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                if (($alt_stem = $stem[0]) =~ s/o($consonant_regex)$/e$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "PlMaNo";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "PlNeNo";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-as";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $stem[0] =~ s/h$//g;
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                if (($alt_stem = $stem[0]) =~ s/o($consonant_regex)$/e$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "PlMaAc";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "PlNeAc";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-as";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $stem[0] =~ s/h$//g;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                if ($stem[0] =~ m/o$consonant_regex$/) { $stem[2] = $stem[0]; $stem[2] =~ s/o($consonant_regex)$/e$1/; }
                $formhash{"function"} = "PlMaGe";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "PlNeGe";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $stem[0] =~ s/h$//g;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                if ($stem[0] =~ m/o$consonant_regex$/) { $stem[2] = $stem[0]; $stem[2] =~ s/o($consonant_regex)$/e$1/; }
                $formhash{"function"} = "PlMaDa";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/cynn/) {
                    $formhash{"function"} = "PlNeDa";
                }    #cynn declines like stan, but is neuter
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #WORD
            }
            elsif ($mywords[$i]{noun_paradigm}[0] =~ m/word/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-es";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "PlNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "PlNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

            }

            #HOF
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/hof/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "SgNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "SgNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-es";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "PlNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "PlNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "PlNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "PlNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

            }

            #DAEG
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/d\x{00E6}g/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-es";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-as";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-as";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

            }

            #FAET
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/f\x{00E6}t/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-es";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[1] = $stem[0];
                $stem[1] =~ s/\x{00E6}/a/;
                $stem[1] =~ s/\x{01FD}/\x{00E1}/;
                $formhash{"function"} = "PlNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #ÁR
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/\x{00E1}r/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "SgFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "SgFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "SgFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "PlFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                if (($alt_stem = $stem[0]) =~ s/($consonant_regex)\1$/$1/) { push(@stem, $alt_stem); }
                $formhash{"function"} = "PlFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "PlFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-na";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ena";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                if ((syllab($stem[0]) > 1) && (($alt_stem = $stem[0]) =~ s/$vowel_regex($consonant_regex)$/$1/)) {
                    push(@stem, $alt_stem);
                }
                $formhash{"function"} = "PlFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #STRENGU
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/strengu/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #FELD & HAND
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/hand|feld/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeNo";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "SgMaNo";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeAc";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "SgMaAc";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeGe";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "SgMaGe";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeDa";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "SgMaDa";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeNo";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "PlMaNo";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeAc";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "PlMaAc";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeGe";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "PlMaGe";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeDa";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/feld/) {
                    $formhash{"function"} = "PlMaGe";
                }    #feld declines like hand , but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #SUNU & DURU
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/sunu|duru/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeNo";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "SgMaNo";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeAc";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "SgMaAc";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeGe";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "SgMaGe";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "SgFeDa";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "SgMaDa";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeNo";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "PlMaNo";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeAc";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "PlMaAc";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeGe";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "PlMaGe";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/u$//;
                $formhash{"function"} = "PlFeDa";
                if ($mywords[$i]{noun_paradigm}[$i2] =~ m/duru/) {
                    $formhash{"function"} = "PlMaGe";
                }    #duru declines like sunu, but is masculine
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #BEARU & THEO(W)
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/bearu/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-wes";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-we";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-was";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-was";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-wa";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-wum";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #BEALU & CNEO(W)
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/bealu/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-wes";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "SgNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-we";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-wa";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[uw]$//;
                $formhash{"function"} = "PlNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-wum";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #WEAK
            %formhash = (
                "title", $mywords[$i]{title},
                "stem", $mywords[$i]{stem},
                "BT", $bt_id, "wordclass", "noun", "class1", "weak", "paradigm", $mywords[$i]{noun_paradigm}[$i2],
                "wright", $mywords[$i]{wright}
            );

            #GUMA
            if ($mywords[$i]{noun_paradigm}[$i2] =~ m/guma/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ena";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #FREA
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/fr\x{00E9}a/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "SgMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ana";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/a$//;
                $formhash{"function"} = "PlMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-am";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-aum";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #TUNGE
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/tunge/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "SgFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "SgFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "SgFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "SgFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "PlFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "PlFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "PlFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ena";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "PlFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-eum";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #BÉO
            if ($mywords[$i]{noun_paradigm}[$i2] =~ m/b\x{00E9}o/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-n";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-n";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-n";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-n";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-n";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-na";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-m";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #ÉAGE
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/\x{00E9}age/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/e$//;
                $formhash{"function"} = "SgNeNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeAc";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeGe";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgNeDa";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlNeNo";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlNeAc";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-an";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ean";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlNeGe";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ena";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlNeDa";
                $stem[0] =~ s/e$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-eum";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #WÍGEND
            elsif ($mywords[$i]{noun_paradigm}[$i2] =~ m/w\x{00ED}gend/) {

                #SgNo
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-es";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #SgDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "SgMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlNo & PlNoAc
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlMaNo";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-as";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlMaAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-as";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-ra";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $formhash{"function"} = "PlMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }
        }
    }

}

#GENERATE ADJECTIVAL FORMS
sub generate_adjforms {
    my @mywords = @_;
    print STDERR "Generating adjectival forms from $#mywords adjectivals.\n";
    my %formhash;
    for my $i (0 .. $#mywords) {
        if (($mywords[$i]{adjective} == 1) || ($mywords[$i]{papart} + $mywords[$i]{pspart} > 0))
        {    #IS IT AN ADJECTIVE OR PARTICIPLE?
            my $bt_id = sprintf("%06d", $mywords[$i]{nid});

            #STRONG
            unless ($mywords[$i]{numeral} == 1) {

  # template :LEMMA | STEM | BT-ID | WORDCLASS | GRADE | CLASS | SUBCLASS | PARADIGM | PARADIGM-ID | WRIGHT | VARIANT-ID
                %formhash = (
                    "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                    "BT",     $bt_id,              "wordclass", "adjective",
                    "class1", "strong",            "paradigm",  $mywords[$i]{adj_paradigm}[0],
                    "wright", $mywords[$i]{wright}
                );
                if ($mywords[$i]{pronoun} == 1) { $formhash{"wordclass"} = "pronoun"; }

                # GLAED & TIL
                if ($mywords[$i]{adj_paradigm}[0] =~ m/(gl\x{00E6}d)|(til)/) {

                    #Sg
                    #Ma
                    #glead needs alternative vowel in certain cases
                    my $title_alt = $mywords[$i]{prefix} . "-" . $mywords[$i]{stem};
                    if ($mywords[$i]{adj_paradigm}[0] =~ m/(\x{00E6})|(\x{00C6})|(ea)/) {
                        $title_alt = $mywords[$i]{stem};
                        $title_alt =~ s/(\x{00E6})|(ea)/a/;
                        $title_alt = $mywords[$i]{prefix} . "-" . $title_alt;
                    }

                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    # BLIND
                }
                elsif ($mywords[$i]{adj_paradigm}[0] =~ m/blind/) {

                    #Sg
                    #Ma
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    # HEAH & THWEORH
                }
                elsif ($mywords[$i]{adj_paradigm}[0] =~ m/(h\x{00E9}ah)|(\x{00FE}weorh)/) {

                    #Sg
                    #Ma
                    my $title_alt = $mywords[$i]{prefix} . "-" . $mywords[$i]{stem};

                    #thweorh lenghthens the diphthong
                    if ($mywords[$i]{adj_paradigm}[0] =~ m/\x{00FE}weorh/) {
                        $title_alt = $mywords[$i]{stem};
                        $title_alt =~ s/e([ao])/\x{00E9}$1/;
                        $title_alt =~ s/([^\x{00E9}])o/$1\x{00F3}/;
                        $title_alt = $mywords[$i]{prefix} . "-" . $title_alt;
                    }
                    $title_alt =~ s/h$//;
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "" . $title_alt . "h-0";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "" . $title_alt . "n-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-s";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-m";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "" . $title_alt . "h-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "" . $title_alt . "h-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-s";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-m";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "" . $title_alt . "-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "" . $title_alt . "r-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "" . $title_alt . "r-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "" . $title_alt . "r-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-m";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "" . $title_alt . "r-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-m";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "" . $title_alt . "r-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-m";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    # MANIG
                }
                elsif (($mywords[$i]{adj_paradigm}[0] =~ m/manig/)
                    || (($mywords[$i]{papart} == 1) && (!&stem_length($mywords[$i]{stem}))))
                {
                    if ($mywords[$i]{papart} == 1) {
                        $mywords[$i]{adj_paradigm}[0] = "manig";
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "strong",            "class2",    "past",
                            "paradigm", "manig",             "wright",    $mywords[$i]{wright}
                        );
                    }

                    #Sg
                    #Ma
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    # HALIG
                }
                elsif (($mywords[$i]{adj_paradigm}[0] =~ m/h\x{00E1}lig/)
                    || (($mywords[$i]{papart} == 1) && (&stem_length($mywords[$i]{stem}))))
                {
                    $title_alt = $mywords[$i]{stem};
                    if ($mywords[$i]{papart} == 1) {
                        $mywords[$i]{adj_paradigm}[0] = "h\x{00E1}lig";
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "strong",            "class2",    "past",
                            "paradigm", "halig",             "wright",    $mywords[$i]{wright}
                        );
                    }
                    else { $title_alt =~ s/($vowel_regex.*)$vowel_regex(.*?)$/$1$2/; }
                    $title_alt = $mywords[$i]{prefix} . "-" . $title_alt;

                    #Sg
                    #Ma
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    # WILDE
                }
                elsif (($mywords[$i]{adj_paradigm}[0] =~ m/wilde/) || ($mywords[$i]{pspart} == 1)) {
                    if ($mywords[$i]{pspart} == 1) {
                        $mywords[$i]{adj_paradigm}[0] = "wilde";
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "strong",            "class2",    "present",
                            "paradigm", "wilde",             "wright",    $mywords[$i]{wright}
                        );
                    }

                    #Sg
                    #Ma
                    $title_alt = $mywords[$i]{stem};
                    $title_alt =~ s/e$//;
                    $title_alt               = $mywords[$i]{prefix} . "-" . $title_alt;
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "$title_alt-e";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ne";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-es";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-re";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-a";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ra";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-um";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    # GEARU
                }
                elsif ($mywords[$i]{adj_paradigm}[0] =~ m/gearu/) {

                    #Sg
                    #Ma
                    $title_alt = $mywords[$i]{stem};
                    $title_alt =~ s/.$//;
                    $title_alt               = $mywords[$i]{prefix} . "-" . $title_alt;
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "0";
                    my $form_parts = "$title_alt-u";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-one";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wes";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-uwes";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaGe";
                    $formhash{"probability"} = "2";
                    $form_parts              = "$title_alt-owes";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wum";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgMaIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-we";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wes";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wum";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgNeIs";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-we";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-we";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ore";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoSgFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ore";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"}    = "PoPlMaNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-e";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ora";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlMaDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wum";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeNo";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-u";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeAc";
                    $formhash{"probability"} = "1";
                    $form_parts              = "$title_alt-o";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ora";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlNeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wum";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wa";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeNo";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-we";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wa";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeAc";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-we";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeGe";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-ora";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"}    = "PoPlFeDa";
                    $formhash{"probability"} = "0";
                    $form_parts              = "$title_alt-wum";
                    $form                    = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                }

                #WEAK

                #create alternative stems
                my @title_array = ();
                my $title_alt   = $mywords[$i]{stem};
                push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                if ($title_alt =~ s/u$/w/)           { push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt); }
                if ($title_alt =~ s/$vowel_regex$//) { push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt); }
                if (   $mywords[$i]{adj_paradigm}[0] =~ m/h\x{00E1}lig/
                    && $mywords[$i]{papart} + $mywords[$i]{pspart} == 0)
                {
                    $title_alt =~ s/($vowel_regex.*)$vowel_regex(.*?)$/$1$2/;
                    push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                }
                if ($title_alt =~ s/h$//) { push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt); }

                #cycle through alt stems
                for my $y (0 .. $#title_array) {

  # template :LEMMA | STEM | BT-ID | WORDCLASS | GRADE | CLASS | SUBCLASS | PARADIGM | PARADIGM-ID | WRIGHT | VARIANT-ID
                    %formhash = (
                        "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                        "BT",     $bt_id,              "wordclass", "adjective",
                        "class1", "weak",              "paradigm",  "blinda",
                        "wright", $mywords[$i]{wright}
                    );
                    if ($mywords[$i]{papart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "weak",              "class2",    "past",
                            "paradigm", "blinda",            "wright",    $mywords[$i]{wright}
                        );
                    }
                    if ($mywords[$i]{pspart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "weak",              "class2",    "present",
                            "paradigm", "blinda",            "wright",    $mywords[$i]{wright}
                        );
                    }
                    if ($mywords[$i]{pronoun} == 1) { $formhash{"wordclass"} = "pronoun"; }

                    $probability = $y;

                    #Sg
                    #Ma
                    $formhash{"function"} = "PoSgMaNo";
                    my $form_parts = "" . $title_array[$y] . "-a";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgMaAc";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgMaGe";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgMaDa";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "PoSgNeNo";
                    $form_parts           = "" . $title_array[$y] . "-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgNeAc";
                    $form_parts           = "" . $title_array[$y] . "-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgNeGe";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgNeDa";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "PoSgFeNo";
                    $form_parts           = "" . $title_array[$y] . "-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgFeAc";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgFeGe";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoSgFeDa";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"} = "PoPlMaNo";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlMaAc";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-ra";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlMaDa";
                    $form_parts           = "" . $title_array[$y] . "-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "PoPlNeNo";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlNeAc";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-ra";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlNeDa";
                    $form_parts           = "" . $title_array[$y] . "-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "PoPlFeNo";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlFeAc";
                    $form_parts           = "" . $title_array[$y] . "-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-ra";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "PoPlFeDa";
                    $form_parts           = "" . $title_array[$y] . "-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                }
            }
            if (   (($mywords[$i]{adjective} == 1) || ($mywords[$i]{papart} + $mywords[$i]{pspart} > 0))
                && ($mywords[$i]{numeral} == 0)
                && ($mywords[$i]{pronoun} == 0))
            {
                #COMPARATIVE

                @title_array = ();
                $c           = "0";

                #irregular comparatives
                if ($mywords[$i]{stem} eq "g\x{00F3}d") {
                    push(@title_array, $mywords[$i]{prefix} . "-beter");
                    push(@title_array, $mywords[$i]{prefix} . "-betr");
                    push(@title_array, $mywords[$i]{prefix} . "-bettr");
                    push(@title_array, $mywords[$i]{prefix} . "-s\x{00E9}lr");
                    push(@title_array, $mywords[$i]{prefix} . "-selr");
                }
                elsif ($mywords[$i]{stem} eq "yfel")  { push(@title_array, $mywords[$i]{prefix} . "-wiers"); }
                elsif ($mywords[$i]{stem} eq "micel") { push(@title_array, $mywords[$i]{prefix} . "-m\x{00E1}r"); }
                elsif ($mywords[$i]{stem} eq "lytel") { push(@title_array, $mywords[$i]{prefix} . "-l\x{01FD}ss"); }
                else {
                    $c = "r";
                    $mywords[$i]{stem} =~ m/($vowel_regex[eao]?)/;
                    my @vowels = &iumlaut($1);
                    for my $y (0 .. $#vowels) {
                        $title_alt = $mywords[$i]{stem};
                        $title_alt =~ s/$vowel_regex[eao]?/$vowels[$y]/;
                        push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                        if ($title_alt =~ s/u$/w/) { push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt); }
                        if ($title_alt =~ s/$vowel_regex$//) {
                            push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                        }
                        if (   $mywords[$i]{adj_paradigm}[0] =~ m/h\x{00E1}lig/
                            && $mywords[$i]{papart} + $mywords[$i]{pspart} == 0)
                        {
                            $title_alt =~ s/($vowel_regex.*)$vowel_regex(.*?)$/$1$2/;
                            push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                        }
                        if ($title_alt =~ s/h$//) { push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt); }
                    }
                }

                #delete duplicities
                my %hash = map { $_, 1 } @title_array;
                @title_array = keys %hash;
                for my $y (0 .. $#title_array) {

   # template :LEMMA | STEM | BT-ID | WORDCLASS | TYPE | CLASS | SUBCLASS | PARADIGM | PARADIGM-ID | WRIGHT | VARIANT-ID
                    my %formhash = (
                        "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                        "BT",     $bt_id,              "wordclass", "adjective",
                        "class1", "weak",              "paradigm",  "blinda",
                        "wright", $mywords[$i]{wright}
                    );
                    if ($mywords[$i]{papart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "weak",              "class2",    "past",
                            "paradigm", "blinda",            "wright",    $mywords[$i]{wright}
                        );
                    }
                    if ($mywords[$i]{pspart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "weak",              "class2",    "present",
                            "paradigm", "blinda",            "wright",    $mywords[$i]{wright}
                        );
                    }

                    $probability = abs($y - 2);

                    #Sg
                    #Ma
                    $formhash{"function"} = "CoSgMaNo";
                    my $form_parts = "" . $title_array[$y] . "-$c-a";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "CoSgNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$c-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$c-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "CoSgFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$c-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoSgFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"} = "CoPlMaNo";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$c-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "CoPlNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$c-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "CoPlFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$c-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$c-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "CoPlFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$c-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                }

                #SUPERLATIVE WEAK
                @title_array = ();
                $s           = "0";

                #irregular superlatives
                if ($mywords[$i]{stem} eq "g\x{00F3}d") {
                    push(@title_array, $mywords[$i]{prefix} . "-betst");
                    push(@title_array, $mywords[$i]{prefix} . "-betest");
                    push(@title_array, $mywords[$i]{prefix} . "-best");
                    push(@title_array, $mywords[$i]{prefix} . "-s\x{00E9}lest");
                }
                elsif ($mywords[$i]{stem} eq "yfel") {
                    push(@title_array, $mywords[$i]{prefix} . "-wierrest");
                    push(@title_array, $mywords[$i]{prefix} . "-wyrst");
                }
                elsif ($mywords[$i]{stem} eq "micel") { push(@title_array, $mywords[$i]{prefix} . "-m\x{01FD}st"); }
                elsif ($mywords[$i]{stem} eq "lytel") { push(@title_array, $mywords[$i]{prefix} . "-l\x{01FD}st"); }
                else {
                    $s = "ost";
                    $mywords[$i]{stem} =~ m/($vowel_regex[eao]?)/;
                    my @vowels = &iumlaut($1);
                    for my $y (0 .. $#vowels) {
                        $title_alt = $mywords[$i]{stem};
                        $title_alt =~ s/$vowel_regex[eao]?/$vowels[$y]/;
                        push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                        if ($title_alt =~ s/u$/w/) { push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt); }
                        if ($title_alt =~ s/$vowel_regex$//) {
                            push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                        }
                        if (   $mywords[$i]{adj_paradigm}[0] =~ m/h\x{00E1}lig/
                            && $mywords[$i]{papart} + $mywords[$i]{pspart} == 0)
                        {
                            $title_alt =~ s/($vowel_regex.*)$vowel_regex(.*?)$/$1$2/;
                            push(@title_array, $mywords[$i]{prefix} . "-" . $title_alt);
                        }

           #if ($title_alt =~ s/($vowel_regex[eao]?)[h]$/$1/) {push(@title_array, $mywords[$i]{prefix}."-".$title_alt);}
                    }
                }

                #delete duplicities

                %hash        = map { $_, 1 } @title_array;
                @title_array = keys %hash;
                for my $y (0 .. $#title_array) {

   # template :LEMMA | STEM | BT-ID | WORDCLASS | TYPE | CLASS | SUBCLASS | PARADIGM | PARADIGM-ID | WRIGHT | VARIANT-ID
                    %formhash = (
                        "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                        "BT",     $bt_id,              "wordclass", "adjective",
                        "class1", "weak",              "paradigm",  "blinda",
                        "wright", $mywords[$i]{wright}
                    );
                    if ($mywords[$i]{papart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "weak",              "class2",    "past",
                            "paradigm", "blinda",            "wright",    $mywords[$i]{wright}
                        );
                    }
                    if ($mywords[$i]{pspart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,              "wordclass", "participle",
                            "class1",   "weak",              "class2",    "present",
                            "paradigm", "blinda",            "wright",    $mywords[$i]{wright}
                        );
                    }

                    $probability = abs($y - 2);

                    #Sg
                    #Ma
                    $formhash{"function"} = "SpSgMaNo";
                    my $form_parts = "" . $title_array[$y] . "-$s-a";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "SpSgNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "SpSgFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"} = "SpPlMaNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "SpPlNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "SpPlFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-an";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-ena";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

   #SUPERLATIVE STRONG
   # template :LEMMA | STEM | BT-ID | WORDCLASS | TYPE | CLASS | SUBCLASS | PARADIGM | PARADIGM-ID | WRIGHT | VARIANT-ID
                    %formhash = (
                        "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                        "BT",     $bt_id,              "wordclass", "adjective",
                        "class1", "strong",            "paradigm",  $mywords[$i]{adj_paradigm}[0],
                        "wright", $mywords[$i]{wright}
                    );
                    if ($mywords[$i]{papart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title},           "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,                        "wordclass", "participle",
                            "class1",   "strong",                      "class2",    "past",
                            "paradigm", $mywords[$i]{adj_paradigm}[0], "wright",    $mywords[$i]{wright}
                        );
                    }
                    if ($mywords[$i]{pspart} == 1) {
                        %formhash = (
                            "title",    $mywords[$i]{title},           "stem",      $mywords[$i]{stem},
                            "BT",       $bt_id,                        "wordclass", "participle",
                            "class1",   "strong",                      "class2",    "present",
                            "paradigm", $mywords[$i]{adj_paradigm}[0], "wright",    $mywords[$i]{wright}
                        );
                    }

                    #Sg
                    #Ma
                    $formhash{"function"} = "SpSgMaNo";
                    my $form_parts = "" . $title_array[$y] . "-$s-0";
                    my $form       = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-ne";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-es";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "SpSgNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-es";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "SpSgFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-re";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-re";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpSgFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});

                    #Pl
                    #Ma
                    $formhash{"function"} = "SpPlMaNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-ra";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlMaDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Ne
                    $formhash{"function"} = "SpPlNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-ra";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 2;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlNeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});

                    #Fe
                    $formhash{"function"} = "SpPlFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeNo";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 2;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-a";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-e";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 1;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeAc";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 2;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-ra";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeGe";
                    $form_parts           = "" . $title_array[$y] . "-$s-0";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}        = $form;
                    $formhash{"formParts"}   = $form_parts;
                    $formhash{"probability"} = $probability + 2;
                    print_form({%formhash});
                    $formhash{"function"} = "SpPlFeDa";
                    $form_parts           = "" . $title_array[$y] . "-$s-um";
                    $form                 = $form_parts;
                    $form       =~ s/[0\-\n]//g;
                    $form_parts =~ s/[\n]//g;
                    $formhash{"form"}      = $form;
                    $formhash{"formParts"} = $form_parts;
                    print_form({%formhash});
                }
            }
        }
    }

}

#GENERATE ADVERBIAL FORMS
sub generate_advforms {
    my @mywords = @_;
    print STDERR "Generating adverbial forms.\n";
    my %formhash;
    for my $i (0 .. $#mywords) {
        if ($mywords[$i]{adverb} == 1) {    #IS IT AN ADVERB?
            my $bt_id = sprintf("%06d", $mywords[$i]{nid});

            # template
            %formhash = (
                "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                "BT",     $bt_id,              "wordclass", "adverb",
                "wright", $mywords[$i]{wright}
            );

            #positive
            $formhash{"function"}    = "Po";
            $formhash{"probability"} = "0";
            $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-0";
            my $form = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});

            #comparative
            #suppletive stems
            if ($mywords[$i]{stem} eq "wel") {
                push(@title_array, $mywords[$i]{prefix} . "-bet");
                push(@title_array, $mywords[$i]{prefix} . "-s\x{00E9}l");
            }
            elsif ($mywords[$i]{stem} eq "yfele") { push(@title_array, $mywords[$i]{prefix} . "-wiers"); }
            elsif ($mywords[$i]{stem} eq "micle") { push(@title_array, $mywords[$i]{prefix} . "-m\x{00E1}"); }
            elsif ($mywords[$i]{stem} eq "lytel") { push(@title_array, $mywords[$i]{prefix} . "-l\x{01FD}s"); }
            else {
                $mywords[$i]{stem} =~ s/e$//i;

                $formhash{"function"}    = "Co";
                $formhash{"probability"} = "0";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-or";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
                $formhash{"function"}    = "Co";
                $formhash{"probability"} = "1";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ur";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
                $formhash{"function"}    = "Co";
                $formhash{"probability"} = "2";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ar";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
            }

            #superlative
            #suppletive stems
            if ($mywords[$i]{stem} eq "wel") {
                push(@title_array, $mywords[$i]{prefix} . "-betst");
                push(@title_array, $mywords[$i]{prefix} . "-best");
                push(@title_array, $mywords[$i]{prefix} . "-s\x{00E9}lest");
            }
            elsif ($mywords[$i]{stem} eq "yfele") {
                push(@title_array, $mywords[$i]{prefix} . "-wierrest");
                push(@title_array, $mywords[$i]{prefix} . "-wyrst");
            }
            elsif ($mywords[$i]{stem} eq "micle") { push(@title_array, $mywords[$i]{prefix} . "-m\x{01FD}st"); }
            elsif ($mywords[$i]{stem} eq "lytel") { push(@title_array, $mywords[$i]{prefix} . "-l\x{01FD}st"); }
            else {
                $formhash{"function"}    = "Su";
                $formhash{"probability"} = "0";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ost";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
                $formhash{"function"}    = "Su";
                $formhash{"probability"} = "1";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ust";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
                $formhash{"function"}    = "Su";
                $formhash{"probability"} = "2";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-ast";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
                $formhash{"function"}    = "Su";
                $formhash{"probability"} = "2";
                $form_parts              = "$mywords[$i]{prefix}-$mywords[$i]{stem}-st";
                $form                    = $form_parts;
                $form       =~ s/[0\-\n]//g;
                $form_parts =~ s/[\n]//g;
                $formhash{"form"}      = $form;
                $formhash{"formParts"} = $form_parts;
                print_form({%formhash});
            }
        }
    }
}

#GENERATE NUMERAL FORMS
sub generate_numforms {
    my @mywords = @_;
    print STDERR "Generating numeral forms.\n";
    my %formhash;
    for my $i (0 .. $#mywords) {
        if ($mywords[$i]{numeral} == 1) {    #IS IT A NUMERAL?
            my $bt_id = sprintf("%06d", $mywords[$i]{nid});

            #CARDINALS AS NOUNS

            if ($mywords[$i]{noun} == 1) {

                %formhash = (
                    "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                    "BT",     $bt_id,              "wordclass", "numeral",
                    "class1", "strong",            "wright",    $mywords[$i]{wright}
                );

                #Masculine
                $formhash{"paradigm"} = "wine";

                #PlNo & PlNoAc
                $formhash{"function"} = "PlMaNo";
                $stem[0] = $mywords[$i]{stem};
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $formhash{"function"} = "PlMaAc";
                $stem[0] = $mywords[$i]{stem};
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlMaGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlMaDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #Feminine
                $formhash{"paradigm"} = "cw\x{00E9}ne";

                #PlNo & PlNoAc
                $formhash{"function"} = "PlFeNo";
                $stem[0] = $mywords[$i]{stem};
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $formhash{"function"} = "PlFeAc";
                $stem[0] = $mywords[$i]{stem};
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlFeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-e";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlFeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlFeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #Neuter
                $formhash{"paradigm"} = "spere";

                #PlNo & PlNoAc
                $formhash{"function"} = "PlNeNo";
                $stem[0] = $mywords[$i]{stem};
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $formhash{"function"} = "PlNeAc";
                $stem[0] = $mywords[$i]{stem};
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlNeAc";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-0";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-u";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-o";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                    }
                }

                #PlGe
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlNeGe";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-a";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }

                #PlDa
                $stem[0] = $mywords[$i]{stem};
                $stem[0] =~ s/[ea]$//;
                $formhash{"function"} = "PlNeDa";
                for my $i3 (0 .. $#stem) {
                    if ($stem[$i3]) {
                        $form_parts = "$mywords[$i]{prefix}-$stem[$i3]-um";
                        $form       = $form_parts;
                        $form =~ s/[0\-\n]//g;
                        $formhash{"form"}      = $form;
                        $formhash{"formParts"} = $form_parts;
                        print_form({%formhash});
                        $stem[$i3] = 0;
                    }
                }
            }

            #CARDINALS AND ORDINALS AS ADJECTIVES

            %formhash = (
                "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                "BT",     $bt_id,              "wordclass", "numeral",
                "class1", "weak",              "paradigm",  "blinda",
                "wright", $mywords[$i]{wright}
            );
            my $alt_title = $mywords[$i]{prefix} . "-" . $mywords[$i]{stem};
            $alt_title =~ s/[eao]$//;

            #Pl
            #Ma
            $formhash{"function"} = "PoPlMaNo";
            $form_parts           = "" . $alt_title . "-an";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlMaAc";
            $form_parts           = "" . $alt_title . "-an";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlMaGe";
            $form_parts           = "" . $alt_title . "-ra";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlMaGe";
            $form_parts           = "" . $alt_title . "-ena";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}        = $form;
            $formhash{"formParts"}   = $form_parts;
            $formhash{"probability"} = $probability + 1;
            print_form({%formhash});
            $formhash{"function"} = "PoPlMaDa";
            $form_parts           = "" . $alt_title . "-um";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});

            #Ne
            $formhash{"function"} = "PoPlNeNo";
            $form_parts           = "" . $alt_title . "-an";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlNeAc";
            $form_parts           = "" . $alt_title . "-an";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlNeGe";
            $form_parts           = "" . $alt_title . "-ra";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlNeGe";
            $form_parts           = "" . $alt_title . "-ena";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}        = $form;
            $formhash{"formParts"}   = $form_parts;
            $formhash{"probability"} = $probability + 1;
            print_form({%formhash});
            $formhash{"function"} = "PoPlNeDa";
            $form_parts           = "" . $alt_title . "-um";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});

            #Fe
            $formhash{"function"} = "PoPlFeNo";
            $form_parts           = "" . $alt_title . "-an";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlFeAc";
            $form_parts           = "" . $alt_title . "-an";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlFeGe";
            $form_parts           = "" . $alt_title . "-ra";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});
            $formhash{"function"} = "PoPlFeGe";
            $form_parts           = "" . $alt_title . "-ena";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}        = $form;
            $formhash{"formParts"}   = $form_parts;
            $formhash{"probability"} = $probability + 1;
            print_form({%formhash});
            $formhash{"function"} = "PoPlFeDa";
            $form_parts           = "" . $alt_title . "-um";
            $form                 = $form_parts;
            $form       =~ s/[0\-\n]//g;
            $form_parts =~ s/[\n]//g;
            $formhash{"form"}      = $form;
            $formhash{"formParts"} = $form_parts;
            print_form({%formhash});

        }
    }
}

#GENERATE VERBALS FORMS
sub generate_vbforms {
    my @mywords = @_;
    print STDERR "Generating verbal forms from $#mywords verbals.\n";
    my %formhash;
    my $count = 0;

    #CYCLE WORDS & PARADIGMS
    for my $i (0 .. $#mywords) {
        foreach (@{ $mywords[$i]{vb_paradigm} }) {
            my $probability;
            my $type     = $_->{type};
            my $class    = $_->{class};
            my $subclass = $_->{subclass};
            my $title    = $_->{title};
            my $ID       = $_->{ID};
            my $wright   = $_->{wright};
            my $bt_id    = sprintf("%06d", $mywords[$i]{nid});

# output template for all verbs:LEMMA | STEM | BT-ID | WORDCLASS | VERB-TYPE | VERB-CLASS | SUBCLASS | PARADIGM | PARADIGM-ID | WRIGHT |
            my $form_template_header =
              "$mywords[$i]{title}\t$mywords[$i]{stem}\t$bt_id\tverb\t$type\t$class\t$subclass\t$title\t$ID\t$wright\t";
            %formhash = (
                "title",  $mywords[$i]{title}, "stem",      $mywords[$i]{stem},
                "BT",     $bt_id,              "wordclass", "verb",
                "class1", $type,               "class2",    $class,
                "class3", $subclass,           "paradigm",  $title,
                "paraID", $ID,                 "wright",    $mywords[$i]{wright}
            );

            #we will need the boundary of the infinitive of the 0 variant for determining post-vowel
            my $boundary_inf = $_->{variant}[0]{if}{boundary};
            my $vowel_inf    = $_->{variant}[0]{if}{vowel};
            my $vowel_pa     = $_->{variant}[0]{painsg1}{vowel};

            #CYCLE VARIANTS
            foreach (@{ $_->{variant} }) {
                my $variantID = $_->{variantID};

                # output template for this variant + VARIANT-ID
                my $form_template_header = $form_template_header . "$variantID\t";
                $formhash{"var"} = $variantID;

                #CYCLE PARADIGM PARTS
                while (my ($name, $item) = each %{$_}) {
                    unless ($name eq "variantID") {

                        #IF A VERB HAS A PREFIX, IT WILL RUN TWICE, ONCE WITH IT, ONCE WITHOUT
                        my $prefix_count;
                        my $prefix = "";
                        $prefix = $mywords[$i]{prefix};
                        if ($prefix ne $item->{prefix}) { $prefix = $prefix . "-" . $item->{prefix}; }
                        my $paraID   = $item->{paraID};
                        my $ending   = $item->{ending};
                        my $boundary = $item->{boundary};
                        my $post_vowel;

                        # POST VOWEL IS DERIVED FROM THE ACTUAL WORD -  if
                        # boundary is defined, look for the postvowel before
                        # boundary, if not look before the ending
                        if ($boundary_inf) {
                            if (
                                ($item->{postVowel})
                                && ($mywords[$i]{stem} =~
                                    m/$vowel_regex$vowel_regex*?($consonant_regex.*?)$boundary_inf$vowel_regex+n$/)
                              )
                            {
                                $post_vowel = $1;
                            }
                            else { $post_vowel = ""; }
                        }
                        elsif (($item->{postVowel})
                            && ($mywords[$i]{stem} =~ m/$vowel_regex$vowel_regex*?($consonant_regex.*?)$vowel_regex+n$/)
                          )
                        {
                            $post_vowel = $1;
                        }
                        else { $post_vowel = ""; }

                        #PREVOWEL IS DERIVED FROM THE ACTUAL WORD
                        $mywords[$i]{stem} =~ m/^($vowel_regex*?.*?)($vowel_regex$vowel_regex?)/;

                        my $pre_vowel = $1;

                        #STRONG VERBS
                        if ($type eq "s") {

#FOR STRONG VERBS, ROOT VOWEL IS DERIVED FROM THE PARADIGM. IF IT THE ROOT VOWEL IS DIFFERENT FROM ACTUAL, IT WILL OUTPUT BOTH VARIANTS
                            my @vowel;
                            $vowel[0] = $item->{vowel};
                            if (!($vowel[0] eq $2) && ($paraID =~ m/^if$/i) && ($variantID == 0)) { $vowel[1] = $2; }

                            #remove empty vowel elements from the array
                            @full_vowel = map { $_ ? $_ : () } @vowel;
                            @vowel      = @full_vowel;

                            for my $vcount (0 .. $#vowel) {

                                #output template for this paradigm member + PARADIGM-PART
                                my $form_header = $form_template_header . "$paraID";
                                $formhash{"function"} = $paraID;

# first output all parts of the paradigm that are defined in the paradigm dictionary (mostly the principal parts)  TEMPLATE + PROBABILITY | FORMPARTS | FORM
                                my $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$ending";
                                my $form       = $form_parts;
                                $form       =~ s/[0\-\n]//g;
                                $form_parts =~ s/[\n]//g;
                                $formhash{"form"}        = $form;
                                $formhash{"formParts"}   = $form_parts;
                                $formhash{"probability"} = $probability;
                                print_form({%formhash});

                                #add generated past participles to the wordlist
                                if (($paraID =~ m/^PaPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                    my $adjsize = $#adjectives + 1;
                                    $adjectives[$adjsize]{prefix} = $prefix;
                                    $adjectives[$adjsize]{stem}   = $form_parts;
                                    $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                    $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                    $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                    $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                    $adjectives[$adjsize]{papart} = 1;
                                }

                                # from Inf we generate: ii, PsPa, PsInSg1, PsInPl, PsSuSg, PsSuPl, ImpSg, ImpPl
                                if ($paraID =~ m/^if$/i) {

                                    #ii  -anne, -enne; -nne
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "IdIf";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-anne";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-enne";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                    }
                                    else {
                                        $formhash{"function"} = "IdIf";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-nne";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsPa -ende; -nde
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "PsPt";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-ende";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }
                                    else {
                                        $formhash{"function"} = "PsPt";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-nde";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #add generated present participles to the wordlist
                                    if ($prefix == $mywords[$i]{prefix}) {
                                        my $adjsize = $#adjectives + 1;
                                        $adjectives[$adjsize]{prefix} = $prefix;
                                        $adjectives[$adjsize]{stem}   = $form_parts;
                                        $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                        $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                        $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                        $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                        $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                        $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                        $adjectives[$adjsize]{pspart} = 1;
                                    }

                                    #PsInSg1 -e, -u, -o, -æ; -0
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "PsInSg1";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-e";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-u";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-o";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-\x{00E6}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                    }
                                    else {
                                        $formhash{"function"} = "PsInSg1";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-0";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsInPl -aþ, -eþ, -as, -es; -þ
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "PsInPl";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-a\x{00FE}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-e\x{00FE}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-es";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-as";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                    }
                                    else {
                                        $formhash{"function"} = "PsInPl";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-\x{00FE}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsSuSg -e; -0
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "PsSuSg";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-e";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }
                                    else {
                                        $formhash{"function"} = "PsSuSg";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-0";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsSuPl -en; -n
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "PsSuPl";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-en";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }
                                    else {
                                        $formhash{"function"} = "PsSuPl";
                                        $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-n";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #ImpSg -0
                                    $formhash{"function"} = "ImSg";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-0";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #ImpPl -aþ; -þ
                                    if ($ending =~ m/an/) {
                                        $formhash{"function"} = "ImPl";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-a\x{00FE}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }
                                    else {
                                        $formhash{"function"} = "ImPl";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-\x{00FE}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    # and by umlaut also: PsInSg2, PsInSg3
                                    my @mvowels = &iumlaut($vowel[$vcount]);
                                    for (my $mvowel_count = 0 ; $mvowel_count < @mvowels ; $mvowel_count++) {
                                        $probability = $prefix_count + $mvowel_count;
                                        $mvowel      = $mvowels[$mvowel_count];

                                        #PsInSg2 -st, -stu, -est, -ist, -s
                                        $formhash{"function"} = "PsInSg2";
                                        $form_parts           = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-stu";
                                        $form                 = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-est";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-ist";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-s";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-st";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #dst > tst
                                        if ($form =~ s/dst$/tst/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #þst > tst
                                        if ($form =~ s/\x{00FE}st$/tst/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #tst > st
                                        if ($form =~ s/tst$/st/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #ngst > ncst
                                        if ($form =~ s/ngst$/ncst/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #ncst > nst
                                        if ($form =~ s/ncst$/nst/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #gst > hst
                                        if ($form =~ s/gst$/hst/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #hst > xst
                                        if ($form =~ s/hst$/xst/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #PsInSg3 -eþ, -iþ, -þ
                                        $formhash{"function"} = "PsInSg3";
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-e\x{00FE}";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-i\x{00FE}";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $form_parts = "$prefix-$pre_vowel-$mvowel-$post_vowel-$boundary-\x{00FE}";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #tþ,dþ > tt
                                        if ($form =~ s/[td]\x{00FE}$/tt/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #tt,dt > t
                                        if ($form =~ s/[dt]t$/t/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #þþ > þ
                                        if ($form =~ s/\x{00FE}\x{00FE}$/\x{00FE}/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #þ > t
                                        if ($form =~ s/\x{00FE}$/t/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #sþ > st
                                        if ($form =~ s/s\x{00FE}$/st/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #ngþ > ncþ
                                        if ($form =~ s/ng\x{00FE}$/nc\x{00FE}/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #gþ > hþ
                                        if ($form =~ s/g\x{00FE}$/h\x{00FE}/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }
                                    }
                                }
                                $probability = $prefix_count;

                                #from PaInSg1 we generate: PaInSg3
                                if ($paraID =~ m/^painsg1$/i) {

                                    #PaInSg3
                                    $formhash{"function"} = "PaInSg3";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-0";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                }

                                #from PaInPl we generate: PaInSg2, PaSuSg, PaSuPl
                                if ($paraID =~ m/^painpl$/i) {

                                    #PaInSg2 -e
                                    $formhash{"function"} = "PaInSg2";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-e";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #PaSuSg -e
                                    $formhash{"function"} = "PaSuSg";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-e";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #PaSuPl -en
                                    $formhash{"function"} = "PaSuPl";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-en";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                }
                            }
                        }

                        #WEAK VERBS
                        if ($type eq "w") {

                            #FOR WEAK VERBS, ROOT VOWEL IS  DERIVED FROM ACTUAL
                            $mywords[$i]{stem} =~ m/^($vowel_regex*?.*?)($vowel_regex$vowel_regex?)/;
                            my @vowel;
                            $vowel[0] = $2;
                            $pre_vowel = $1;
                            my $dental = $item->{dental};

                            #output template for this paradigm member + PARADIGM-PART
                            my $form_header = $form_template_header . "$paraID";
                            $formhash{"function"} = $paraID;

                            # class 3 is manually defined
                            if (($ID > 88) && ($ID < 93)) {
                                my $vowel       = $item->{vowel};
                                my $boundary    = $item->{boundary};
                                my $dental      = $item->{dental};
                                my $ending      = $item->{ending};
                                my $post_vowel  = $item->{postVowel};
                                my $pre_vowel   = $item->{preVowel};
                                my $form_header = $form_template_header . "$item->{paraID}";
                                $formhash{"function"} = $paraID;
                                my $probability = $prefix_count;
                                my $form_parts  = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-$dental-$ending";
                                my $form        = $form_parts;
                                $form       =~ s/[0\-\n]//g;
                                $form_parts =~ s/[\n]//g;
                                $formhash{"form"}        = $form;
                                $formhash{"formParts"}   = $form_parts;
                                $formhash{"probability"} = $probability;
                                print_form({%formhash});

                                #add generated present participles to the wordlist
                                if (($paraID =~ m/^PsPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                    my $adjsize = $#adjectives + 1;
                                    $adjectives[$adjsize]{prefix} = $prefix;
                                    $adjectives[$adjsize]{stem}   = $form_parts;
                                    $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                    $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                    $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                    $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                    $adjectives[$adjsize]{pspart} = 1;
                                }

                                #add generated past participles to the wordlist
                                if (($paraID =~ m/^PaPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                    my $adjsize = $#adjectives + 1;
                                    $adjectives[$adjsize]{prefix} = $prefix;
                                    $adjectives[$adjsize]{stem}   = $form_parts;
                                    $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                    $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                    $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                    $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                    $adjectives[$adjsize]{papart} = 1;
                                }
                            }
                            else {

# first output all parts of the paradigm that are defined in the paradigm dictionary (mostly the principal parts)  TEMPLATE + PROBABILITY | FORMPARTS | FORM
                                $form_parts =
                                  "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-$ending";
                                $form = $form_parts;
                                $form       =~ s/[0\-\n]//g;
                                $form_parts =~ s/[\n]//g;
                                $formhash{"form"}        = $form;
                                $formhash{"formParts"}   = $form_parts;
                                $formhash{"probability"} = $probability;
                                print_form({%formhash});

                                #add generated present participles to the wordlist
                                if (($paraID =~ m/^PsPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                    my $adjsize = $#adjectives + 1;
                                    $adjectives[$adjsize]{prefix} = $prefix;
                                    $adjectives[$adjsize]{stem}   = $form_parts;
                                    $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                    $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                    $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                    $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                    $adjectives[$adjsize]{pspart} = 1;
                                }

                                #add generated past participles to the wordlist
                                if (($paraID =~ m/^PaPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                    my $adjsize = $#adjectives + 1;
                                    $adjectives[$adjsize]{prefix} = $prefix;
                                    $adjectives[$adjsize]{stem}   = $form_parts;
                                    $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                    $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                    $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                    $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                    $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                    $adjectives[$adjsize]{papart} = 1;
                                }

                                # parts derived from inf
                                if ($paraID =~ m/^if$/i) {
                                    $iending = "";
                                    if ($ending =~ m/^i/i) { $iending = "i"; }

                                    #Inf -an/-ian
                                    $inf_ending   = $ending;
                                    $inf_boundary = $item->{boundary};

                                    $formhash{"function"} = "if";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$ending";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #If the form ends in a vowel, the initial vowel of the ending is deleted
                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-n";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #ii -anne, -enne; -nne
                                    $formhash{"function"} = "IdIf";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-anne";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-enne";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-nne";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsInSg1 -e, -u, -o, -æ;
                                    $formhash{"function"} = "PsInSg1";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-e";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-u";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-o";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts =
                                      "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-\x{00E6}";
                                    $form = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-0";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsInPl -aþ, -eþ, -as, -es;
                                    $formhash{"function"} = "PsInPl";
                                    $form_parts =
                                      "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-a\x{00FE}";
                                    $form = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                    $form_parts =
                                      "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-e\x{00FE}";
                                    $form = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-es";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-as";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-\x{00FE}";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsSuSg -e
                                    $formhash{"function"} = "PsSuSg";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-e";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-0";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsSuPl -en
                                    $formhash{"function"} = "PsSuPl";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-en";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~ m/$vowel_regex$/) {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-n";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #ImpPl -aþ
                                    $formhash{"function"} = "ImPl";
                                    $form_parts =
                                      "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-a\x{00FE}";
                                    $form = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-\x{00FE}";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #PsPa -ende
                                    $formhash{"function"} = "PsPt";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-ende";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    if ("$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary" =~
m/[\x{00E6}aeyou\x{00C6}AEIYOU\x{01FD}\x{00E1}\x{00E9}\x{00ED}\x{00FD}\x{00F3}\x{00FA}\x{01FC}\x{00C1}\x{00C9}\x{00CD}\x{00DD}\x{00D3}\x{00DA}][\-0]*?$/
                                      )
                                    {
                                        $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-$iending-nde";
                                        $form       = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                    }

                                    #add generated present participles to the wordlist
                                    if ($prefix == $mywords[$i]{prefix}) {
                                        my $adjsize = $#adjectives + 1;
                                        $adjectives[$adjsize]{prefix} = $prefix;
                                        $adjectives[$adjsize]{stem}   = $form_parts;
                                        $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                        $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                        $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                        $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                        $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                        $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                        $adjectives[$adjsize]{pspart} = 1;
                                    }
                                }

                                # parts derived from PsInSg2
                                if ($paraID =~ m/^psinsg2$/i) {

                                    #all verbs simplify double post-vowel in these paradigm parts
                                    $post_vowel =~ s/(.)\1/$1/;

                   #PsInSg2 -st -est, -es, -ist, -s (sealfian and those manually defined have -ast/-as in the para dict)
                                    $formhash{"function"} = "PsInSg2";
                                    $post_vowel = $post_vowel;

                                    #if ($item->{postVowel} ne $inf_post_vowel) {$post_vowel =~ s/(.).$/$1/;}
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-est";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-es";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-ist";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-s";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-st";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #dst > tst
                                    if ($form =~ s/dst$/tst/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #þst > tst
                                    if ($form =~ s/\x{00FE}st$/tst/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #tst > st
                                    if ($form =~ s/tst$/st/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #ngst > ncst
                                    if ($form =~ s/ngst$/ncst/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #ncst > nst
                                    if ($form =~ s/ncst$/nst/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #gst > hst
                                    if ($form =~ s/gst$/hst/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #hst > xst
                                    if ($form =~ s/hst$/xst/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                        #PsInSg3 -eþ, -ieþ, -iþ, -þ (sealfian and those manually defined have -aþ in the para dict)
                                    $formhash{"function"} = "PsInSg3";
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-e\x{00FE}";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-ie\x{00FE}";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-i\x{00FE}";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-\x{00FE}";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});

                                    #tþ,dþ > tt
                                    if ($form =~ s/[td]\x{00FE}$/tt/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #tt > t
                                    if ($form =~ s/tt$/t/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #þþ > þ
                                    if ($form =~ s/\x{00FE}\x{00FE}$/\x{00FE}/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #þ > t
                                    if ($form =~ s/\x{00FE}$/t/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #sþ > st
                                    if ($form =~ s/s\x{00FE}$/st/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #ngþ > ncþ
                                    if ($form =~ s/ng\x{00FE}$/nc\x{00FE}/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #gþ > hþ
                                    if ($form =~ s/g\x{00FE}$/h\x{00FE}/g) {
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});
                                        $count++;
                                    }

                                    #ImpSg -e;-0 (sealfian and those manually defined have -a in the para dict)
                                    $formhash{"function"} = "ImSg";
                                    $form_parts           = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-e";
                                    $form                 = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-ie";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel[0]-$post_vowel-$boundary-0";
                                    $form       = $form_parts;
                                    $form =~ s/[0\-]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                }

                                # parts derived from PaInSg1

                                if ($paraID =~ m/^painsg1$/i) {

                                    #all verbs simplify double post-vowel in these paradigm parts
                                    $post_vowel =~ s/(.)\1/$1/;

#if the preterite vowel for the paradigm example differs from the vowel of the preterite, output with both with the inf vowel as less probable
                                    if ($vowel_inf ne $vowel_pa) { unshift(@vowel, $vowel_pa); }
                                    for my $vcount (0 .. $#vowel) {
                                        $probability = $probability + $vcount;

                                        #PaInSg1 -ed-e
                                        $formhash{"function"} = "PaInSg1";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-e";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #PaInSg2 -ed-est, -ed-es
                                        $formhash{"function"} = "PaInSg2";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-est";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-es";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability + 1;
                                        print_form({%formhash});

                                        #PaInSg3 -ed-e
                                        $formhash{"function"} = "PaInSg3";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-e";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #PaInPl -ed-on
                                        $formhash{"function"} = "PaInPl";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-on";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #PaSuSg -ed-e
                                        $formhash{"function"} = "PaSuSg";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-e";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #PaSuPl -ed-en
                                        $formhash{"function"} = "PaSuPl";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental-en";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

                                        #PaPa -ed
                                        $formhash{"function"} = "PaPt";
                                        $form_parts =
                                          "$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental";
                                        $form = $form_parts;
                                        $form =~ s/[0\-]//g;
                                        $formhash{"form"}        = $form;
                                        $formhash{"formParts"}   = $form_parts;
                                        $formhash{"probability"} = $probability;
                                        print_form({%formhash});

#$form_parts = "ge-$prefix-$pre_vowel-$vowel[$vcount]-$post_vowel-$boundary-$dental";
#$form = $form_parts; $form =~ s/[0\-]//g; $formhash{"form"}=$form; $formhash{"formParts"}=$form_parts; $formhash{"probability"}=$probability+1; push (@forms, {%formhash});
#t-ed > t-t
                                        if ($form =~ s/ted$/tt/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #t-t > t
                                        if ($form =~ s/tt$/t/g) {
                                            $formhash{"form"}        = $form;
                                            $formhash{"formParts"}   = $form_parts;
                                            $formhash{"probability"} = $probability + 1;
                                            print_form({%formhash});
                                            $count++;
                                        }

                                        #add generated past participles to the wordlist
                                        if ($prefix == $mywords[$i]{prefix}) {
                                            my $adjsize = $#adjectives + 1;
                                            $adjectives[$adjsize]{prefix} = $prefix;
                                            $adjectives[$adjsize]{stem}   = $form_parts;
                                            $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                            $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                            $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                            $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                            $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                            $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                            $adjectives[$adjsize]{papart} = 1;
                                        }

                                    }
                                }
                            }
                        }

                        #PRETERITE-PRESENT & ANOMALOUS VERBS
                        if (($type eq "pp") || ($type eq "a")) {
                            my $vowel       = $item->{vowel};
                            my $boundary    = $item->{boundary};
                            my $dental      = $item->{dental};
                            my $ending      = $item->{ending};
                            my $post_vowel  = $item->{postVowel};
                            my $pre_vowel   = $item->{preVowel};
                            my $form_header = $form_template_header . "$item->{paraID}";
                            $formhash{"function"} = $paraID;
                            my $probability = $prefix_count;
                            my $form_parts  = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-$dental-$ending";
                            my $form        = $form_parts;
                            $form       =~ s/[0\-\n]//g;
                            $form_parts =~ s/[\n]//g;
                            $formhash{"form"}        = $form;
                            $formhash{"formParts"}   = $form_parts;
                            $formhash{"probability"} = $probability;
                            print_form({%formhash});

                            #add generated present participles to the wordlist
                            if (($paraID =~ m/^PsPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                my $adjsize = $#adjectives + 1;
                                $adjectives[$adjsize]{prefix} = $prefix;
                                $adjectives[$adjsize]{stem}   = $form_parts;
                                $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                $adjectives[$adjsize]{pspart} = 1;
                            }

                            #add generated past participles to the wordlist
                            if (($paraID =~ m/^PaPt$/i) && ($prefix == $mywords[$i]{prefix})) {
                                my $adjsize = $#adjectives + 1;
                                $adjectives[$adjsize]{prefix} = $prefix;
                                $adjectives[$adjsize]{stem}   = $form_parts;
                                $adjectives[$adjsize]{stem}   = $1 if $form_parts =~ m/$prefix(.*)$/;
                                $adjectives[$adjsize]{stem} =~ s/[0\-\n]//g;
                                $adjectives[$adjsize]{title} = $prefix . $adjectives[$adjsize]{stem};
                                $adjectives[$adjsize]{title} =~ s/[0\-\n]//g;
                                $adjectives[$adjsize]{nid}    = $mywords[$i]{nid};
                                $adjectives[$adjsize]{wright} = $mywords[$i]{wright};
                                $adjectives[$adjsize]{papart} = 1;
                            }

                            # automatically ad suffixes to 5 forms for pp
                            if ($type eq "pp") {
                                if ($paraID =~ m/^if$/i) {

                                    #ii -anne
                                    $formhash{"function"} = "IdIf";
                                    $form_parts           = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-anne";
                                    $form                 = $form_parts;
                                    $form       =~ s/[0\-\n]//g;
                                    $form_parts =~ s/[\n]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                }
                                if ($paraID =~ m/^painsg1$/i) {

                                    #PaInSg2 -est, es
                                    $formhash{"function"} = "PaInSg2";
                                    $form_parts           = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-est";
                                    $form                 = $form_parts;
                                    $form       =~ s/[0\-\n]//g;
                                    $form_parts =~ s/[\n]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                    $form_parts = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-es";
                                    $form       = $form_parts;
                                    $form       =~ s/[0\-\n]//g;
                                    $form_parts =~ s/[\n]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability + 1;
                                    print_form({%formhash});

                                    #PaInSg3 -e
                                    $formhash{"function"} = "PaInSg3";
                                    $form_parts           = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-e";
                                    $form                 = $form_parts;
                                    $form       =~ s/[0\-\n]//g;
                                    $form_parts =~ s/[\n]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #PaSuSg -e
                                    $formhash{"function"} = "PaSuSg";
                                    $form_parts           = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-e";
                                    $form                 = $form_parts;
                                    $form       =~ s/[0\-\n]//g;
                                    $form_parts =~ s/[\n]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});

                                    #PaSuPl -en
                                    $formhash{"function"} = "PaSuPl";
                                    $form_parts           = "$prefix-$pre_vowel-$vowel-$post_vowel-$boundary-en";
                                    $form                 = $form_parts;
                                    $form       =~ s/[0\-\n]//g;
                                    $form_parts =~ s/[\n]//g;
                                    $formhash{"form"}        = $form;
                                    $formhash{"formParts"}   = $form_parts;
                                    $formhash{"probability"} = $probability;
                                    print_form({%formhash});
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

sub print_form {
    my ($form) = @_;
    my $formi = $form->{form};
    $formi =~ s/(y)|(ie?)/i/g;
    $formi =~ s/\x{00FD}|\x{00ED}e/\x{00ED}/g;
    $formi = Unicode::Normalize::NFKD($formi);
    $formi =~ s/\p{NonspacingMark}//g;
    print "$formi\t$form->{BT}\t$form->{title}\t$form->{stem}\t$form->{form}\t$form->{formParts}\t$form->{var}\t$form->{probability}\t$form->{function}\t$form->{wright}\t$form->{paradigm}\t$form->{paraID}\t$form->{wordclass}\t$form->{class1}\t$form->{class2}\t$form->{class3}\t$form->{comment}\n";
    $counter++;

    #remove double consonants from formi with lower probability
    if ($formi =~ s/($consonant_regex)\1/$1/g) {
        $form->{probability}++;
        print "$formi\t$form->{BT}\t$form->{title}\t$form->{stem}\t$form->{form}\t$form->{formParts}\t$form->{var}\t$form->{probability}\t$form->{function}\t$form->{wright}\t$form->{paradigm}\t$form->{paraID}\t$form->{wordclass}\t$form->{class1}\t$form->{class2}\t$form->{class3}\t$form->{comment}\n";
    }
}

#- RUN THE SUBROUTINES ------------------------------------------------------------------------------------------

@forms;

my %files = (
    'dictionary'       => "dict_adj-vb-part-num-adv-noun.txt",
    'manual-forms'     => "manual_forms.txt",
    'verbal-paradigms' => "para_vb.txt",
    'prefixes'         => "prefixes.txt",
    'output'           => "output.txt"
);
GetOptions(\%files, 'dictionary=s', 'manual-forms=s', 'verbal-paradigms=s', 'prefixes=s', 'output=s');

my $start = time();
print STDERR "Printing manual forms...\n";
print_manual_forms($args->{"manual-forms"});
print STDERR "Loading the dictionary...\n";
my @words = load_dictionary($args->{dictionary});
print STDERR "Loading the paradigms...\n";
my $vparadigms = load_paradigms($args->{"verbal-paradigms"});

set_constants();

@words = &remove_prefix(@words);
@words = &remove_hyphens(@words);
@words = &count_syllables(@words);

print STDERR "Setting verbal paradigms...\n";
@verbs = &set_verb_paradigm(\@words, $vparadigms);
print STDERR "Setting adjectival paradigms...\n";
@adjectives = &set_adj_paradigm(@words);
print STDERR "\nSetting nominal paradigms...\n";
@nouns = &set_noun_paradigm(@words);

generate_vbforms(@verbs);
generate_adjforms(@adjectives);
generate_advforms(@words);
generate_numforms(@words);
generate_nounforms(@nouns);

print STDERR "Finished in ${elapsed}s.\n";
