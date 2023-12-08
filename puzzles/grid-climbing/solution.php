<?php declare (strict_types = 1);

$start = microtime(true);

fscanf(STDIN, "%d", $n);
$costs = stream_get_line(STDIN, 256 + 1, "\n");
$costs = explode(" ", $costs);
$grid = [];
for ($i = 0; $i < $n; $i++) {
    $grid[] = stream_get_line(STDIN, $n + 1, "\n");
}

$nodes = [];
for ($x = 0; $x < $n; $x++) for ($y = 0; $y < $n; $y++) {
    $nodes["$x,$y"] = [$x, $y] == [0, 0] ? $grid[0][0] : INF;
}

[$x1, $y1] = [0, 0];

while ([$x1, $y1] != [$n - 1, $n - 1]) {
    # Explore nodes
    foreach ($nodes as $key => $value) {
        [$x2, $y2] = explode(',', $key);
        if ([$x1, $y1] == [$x2, $y2]) continue;
        $cost = $costs[max(abs($x2 - $x1), abs($y2 - $y1)) - 1];
        $new_cost = $nodes["$x1,$y1"] + $grid[$y2][$x2] + $cost;
        $nodes["$x2,$y2"] = min($nodes["$x2,$y2"], $new_cost);
    }
    
    # Remove explored node
    unset($nodes["$x1,$y1"]);

    # Choose node with the current min distance
    $min_key = array_search(min($nodes), $nodes);
    [$x1, $y1] = explode(',', $min_key); 
}

echo $nodes[($n - 1) . ',' . ($n - 1)];
error_log(var_export(microtime(true) - $start, true));
