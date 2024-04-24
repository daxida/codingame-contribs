# 53 (chicken)
$_=`tr -cd A-Z`;print y/Z//,$/while s!..!$&=~Z?Z:0!ge

# 57
$_=`tr -cd A-Z`;s!..!$&=~Z?Z:0!ge,print y/Z//,$/while/../