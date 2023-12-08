# Solutions by @tssw

read n
fold -$n|awk 'NR<2{s="/"}NR>1{s=s","}{s=s length($0)"H"$0}END{print s"/"}'

read n
fold -$n|awk '$0=length($0)"H"$0'|tr \\n ,|sed 's!\(.*\),!/\1/!'