<?php

fscanf(STDIN, "%d", $n);
$c = stream_get_line(STDIN, 1 + 1, "\n");
$s = stream_get_line(STDIN, 100 + 1, "\n");
$s = explode(" ", $s);
$s[0] = "";

$g = [];
for ($i = 0; $i < $n; $i++) {
    $g[] = str_repeat($c, $i);
}

sort($s);

if ($g === $s) {
    echo $n;
} else {
    $needed = array_diff($g, $s);
    echo implode(" ", $needed);
}
