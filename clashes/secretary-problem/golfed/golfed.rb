# 56 @Whale solution but with index

eval"N,*X="+`tr ' 
' ,`
p X.index{_1>X[0,N/2.7].max}||-1

# @Whale 61

eval"N,*X="+`tr ' 
' ,`
p X.find_index{_1>X[0,N/2.7].max}||-1

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
