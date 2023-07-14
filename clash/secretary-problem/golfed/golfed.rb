# 70

eval"N,*A="+`tr ' 
' ,`
c=N*10/27
p (c...N).find{A[_1]>A[0,c].max}||-1

# 72

eval"N,*A="+`tr ' 
' ,`
c=N.div 2.7
p (c...N).find{A[_1]>A[0,c].max}||-1
