<?php

$s = str_split(stream_get_line(STDIN, 256 + 1, "\n"));

$sum = 0;
foreach ($s as $c) {
    $sum += ord($c);
}

$result = 'prime';
foreach ($s as $c) {
    if ($sum % ord($c) == 0) {
        $result = $c;
        break;
    };
}
echo $result . "\n";

?>