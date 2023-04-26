# Solution by @tssw

read n
fold -$n|awk 'NR<2{s="/"}NR>1{s=s","}{s=s length($0)"H"$0}END{print s"/"}'