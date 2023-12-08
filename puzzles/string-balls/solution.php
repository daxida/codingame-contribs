<?php

fscanf(STDIN, "%d", $radius);
$center = stream_get_line(STDIN, 20 + 1, "\n");
$center = array_map(function($char) {
    return ord($char) - 97;
}, str_split($center));

$cur = [$radius => 1];

foreach ($center as $c1) {
    $nxt = [];
    foreach ($cur as $key => $occ) {
        for ($c2 = 0; $c2 < 26; $c2++) {
            $t = $key - abs($c1 - $c2);
            if ($t >= 0) {
                if (!isset($nxt[$t])) $nxt[$t] = 0;
                $nxt[$t] += $occ;
            }
        }
    }
    $cur = $nxt;
}

echo array_sum($cur);
?>