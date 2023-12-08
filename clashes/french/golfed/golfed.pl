# 50

`dd`=~s/[A-ÿ]*é([dfrz]s?\b|x)[A-ÿ]*/print"$&
"/ger

# 52 

$,=$/;print grep{/é[dfrz]s?$|éx/}glob`tr -c A-ÿ ' '`

# 53

$_=`dd`;print"$&
"while/[A-ÿ]*é([dfrz]s?\b|x)[A-ÿ]*/g

# 54

print"$_
"for grep{/é[dfrz]s?$|éx/}glob`tr -c A-ÿ ' '

# 55

$,=$/;print grep{/é[dfrz]s?$|éx/}glob`tr [:punct:] ' '`

# 59

$,=$/;print grep{/é[dfrz]s?$|éx/}split/[\p{Punct} \n]/,`dd`