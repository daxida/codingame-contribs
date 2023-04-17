<?php

function solve($original, $words, $used) {
    global $solution;
    if (empty($words)) {
        if (!empty($solution)) {
            echo "Unsolvable\n";
            exit(0);
        }
        $solution = implode(" ", $used);
    }

    $uniqueWords = array_unique($words);
    foreach ($uniqueWords as $word) {
        if (strpos($original, $word) === 0) {
            $nwords = $words;
            array_splice($nwords, array_search($word, $nwords), 1);
            array_push($used, $word);
            solve(substr($original, strlen($word)), $nwords, $used);
            array_pop($used);
        }
    }
}

$original = stream_get_line(STDIN, 1000 + 1, "\n");
$words = explode(" ", stream_get_line(STDIN, 1000 + 1, "\n"));
$solution = "";
solve($original, $words, []);
echo "$solution\n";

?>
