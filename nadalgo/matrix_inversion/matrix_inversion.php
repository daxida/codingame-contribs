<?php

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