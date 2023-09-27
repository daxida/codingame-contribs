<?php

/*
https://en.wikipedia.org/wiki/Absorbing_Markov_chain
https://github.com/daxida/CodinGame-contributions/blob/main/markov-ants/Tran_presentation.pdf
(FRENCH) https://idpoisson.fr/berglund/probamass_html/node18.html
*/

$DIRS = [[-1, 0], [0, 1], [0, -1], [1, 0]];

function solve($start, int $step, int $w, int $h): float {
    $states = generate_states($step, $w, $h);
    $matrix_size = count($states);
    $matrix = transition_markov($states, $matrix_size);

    // The fundamental matrix is the identity minus the transition
    for ($i = 0; $i < $matrix_size; $i++) {
        for ($j = 0; $j < $matrix_size; $j++) {
            $fundamental[$j][$i] = -$matrix[$j][$i];
            if ($i == $j) {
                $fundamental[$j][$i]++;
            }
        }
    }

    $inv = invert_matrix($fundamental);

    list($x, $y) = $start;
    $row_start_point = array_search("$x,$y", array_keys($states));
    $expectancy = array_sum($inv[$row_start_point]);

    return $expectancy;
}

function generate_states(int $step, int $w, int $h) {
    $states = [];
    for ($y = 1; $y < $h; $y++) {
        for ($x = 1; $x < $w; $x++) {
            foreach ($GLOBALS['DIRS'] as $dir) {
                list($dy, $dx) = $dir;
                $nx = $x + $dx * $step;
                $ny = $y + $dy * $step;
                if (abs($nx) < $w - 1 && abs($ny) < $h - 1) {
                    $states["$x,$y"][] = "$nx,$ny";
                }
            }
        }
    }

    return $states;
}

function transition_markov($states, int $matrix_size) {
    $matrix = array_fill(0, $matrix_size, array_fill(0, $matrix_size, 0));
    $statesKeys = array_keys($states);
    foreach ($statesKeys as $idx1 => $pt1) {
        foreach ($statesKeys as $idx2 => $pt2) {
            if (in_array($pt2, $states[$pt1])) {
                $matrix[$idx1][$idx2] = 1 / 4;
            }
        }
    }

    return $matrix;
}

function main() {
    fscanf(STDIN, "%d", $step);
    fscanf(STDIN, "%d %d", $w, $h);
    $g = [];
    for ($i = 0; $i < $h; $i++) {
        $g[] = trim(fgets(STDIN));
    }
    $start = [];
    for ($y = 0; $y < $h; $y++) {
        for ($x = 0; $x < $w; $x++) {
            if ($g[$y][$x] == "A") {
                $start = array($x, $y);
                break 2;
            }
        }
    }

    $expectancy = solve($start, $step, $w, $h);
    printf("%.1f", $expectancy);
}

main();

// 
// Utility function to invert matrices.
// 

function invert_matrix($matrix) {
    $n = count($matrix);

    $identity = [];
    for ($i = 0; $i < $n; $i++) {
        $identityRow = [];
        for ($j = 0; $j < $n; $j++) {
            $identityRow[] = ($i == $j) ? 1 : 0;
        }
        $identity[] = $identityRow;
    }

    for ($i = 0; $i < $n; $i++) {
        for ($j = 0; $j < $n; $j++) {
            $matrix[$i][] = $identity[$i][$j];
        }
    }

    for ($i = 0; $i < $n; $i++) {
        $pivot = $matrix[$i][$i];

        for ($j = 0; $j < 2 * $n; $j++) {
            $matrix[$i][$j] /= $pivot;
        }

        for ($j = 0; $j < $n; $j++) {
            if ($j !== $i) {
                $factor = $matrix[$j][$i];
                for ($k = 0; $k < 2 * $n; $k++) {
                    $matrix[$j][$k] -= $factor * $matrix[$i][$k];
                }
            }
        }
    }

    $inverseMatrix = [];
    for ($i = 0; $i < $n; $i++) {
        $inverseMatrix[] = array_slice($matrix[$i], $n);
    }

    return $inverseMatrix;
}
