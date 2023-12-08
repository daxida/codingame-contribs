<?php

$a = stream_get_line(STDIN, 5000 + 1, "\n");
$b = stream_get_line(STDIN, 1000 + 1, "\n");
$a = str_split($a);
$b = str_split($b);

$j = 0;
foreach ($a as $v) {
    echo $v == $b[$j] && ++$j ? $v : "-";
}
