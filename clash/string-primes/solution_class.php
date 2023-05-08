<?php

class Solver {
    public function __construct($s) {
        $this->str = $s;
        $this->sz = strlen($s);
        $this->sum = 0;
    }

    public function get_sum() {
        $tmp = 0;
        for ($i = 0; $i < $this->sz; $i++) {
            $tmp += ord($this->str[$i]);
        }
        $this->sum = $tmp;
    }

    public function solve() {
        for ($i = 0; $i < $this->sz; $i++) {
            $cur = ord($this->str[$i]);
            if ($this->sum % $cur == 0) {
                return $this->str[$i];
            }
        }
        return "prime";
    }
}

function main() {
    $s = stream_get_line(STDIN, 256 + 1, "\n");
    $solver = new Solver($s);
    $solver->get_sum();
    echo $solver->solve();
}

main();

?>