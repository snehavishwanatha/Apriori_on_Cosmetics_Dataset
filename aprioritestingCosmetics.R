library(arules)
library(arulesViz)
library(RColorBrewer)

f=file.choose()
f1=read.csv(f)

#summarizing data
data<-f1
#data=na.omit(f1)
head(data,n=10)
str(data)
summary(data)

#inspecting rules
rules<-apriori(data,parameter=list(supp=0.5,conf=0.8,target="rules"))
rules.sorted <-sort(rules, by="confidence")
rules <- rules.sorted[!is.redundant(rules)]

summary(rules)
inspect(rules)

#item frequency histogram
rules<-apriori(data,parameter=list(supp=0.5,conf=0.8,target="rules"))
itemFrequencyPlot(items(rules),topN=13,col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")

#graphical understanding
plot(rules[1:25],method = "graph",cex=0.7,main="Graphical representation for 25 rules")

#reporting the client in layman terms

sink("cosmeticsreport.txt")
suppyes=vector()
supportyes=vector()
suppno=vector()
supportno=vector()
for(q in 1:ncol(data))
{
  suppyes[q]=0
  suppno[q]=0
}
#suppyes
#suppno
for(i in 1:ncol(data))
{ 
  for(j in 1:nrow(data))
  {
    if(data[j,i]=="No")
      suppno[i]=suppno[i]+1
    if(data[j,i]=="Yes")
      suppyes[i]=suppyes[i]+1
  }
  supportyes[i]=suppyes[i]/nrow(data)
  supportno[i]=suppno[i]/nrow(data)

  if(supportyes[i]>0.5) 
    print(paste("Probability of the product ",colnames(data[i])," being purchased is ",supportyes[i]*100,"%"))
}
#suppno
#supportno
#suppyes
#supportyes


op=vector()
sop=vector()
for(l in 1:ncol(data[,-1]))
{  s=l+1
for(k in s:ncol(data))
{
  if(l!=k)
  {
    for(opq in 1:4)
      op[opq]=0
    for(j in 1:nrow(data))
    { 
      if(data[j,l]=="No"&&data[j,k]=="No")
      {
        op[1]=op[1]+1
      }
      else if(data[j,l]=="Yes"&&data[j,k]=="No")
      {
        op[2]=op[2]+1
      }
      else if(data[j,l]=="No"&&data[j,k]=="Yes")
      {
        op[3]=op[3]+1
      }
      else op[4]=op[4]+1
    }
    
    sop=op
    
    for(b in 1:4)
    {
      if(op[b]!=0)
        op[b]=op[b]/nrow(data)
    }
        if(sop[4]!=0&&op[4]>0.5&&suppyes[l]!=0)
    {
      sop[4]=sop[4]/suppyes[l] 
      print(paste(" The probability that purchase of ",colnames(data[l])," influences the purchase of ",colnames(data[k])," when placed on the same aisle is"))
      print(paste(sop[4]*100,"%"))
    }
  }
}
}



op=vector()
sop=vector()
for(l in ncol(data[,-1]):2)
{  s=l-1
for(k in s:ncol(data))
{
  if(l!=k)
  {
    for(opq in 1:4)
      op[opq]=0
    for(j in 1:nrow(data))
    { 
      if(data[j,l]=="No"&&data[j,k]=="No")
      {
        op[1]=op[1]+1
      }
      else if(data[j,l]=="Yes"&&data[j,k]=="No")
      {
        op[2]=op[2]+1
      }
      else if(data[j,l]=="No"&&data[j,k]=="Yes")
      {
        op[3]=op[3]+1
      }
      else op[4]=op[4]+1
    }
    
    sop=op
    
    for(b in 1:4)
    {
      if(op[b]!=0)
        op[b]=op[b]/nrow(data)
    }
    if(sop[4]!=0&&op[4]>0.6&&suppyes[l]!=0)
    {
      sop[4]=sop[4]/suppyes[l] 
      print(paste(" The probability that purcahse of ",colnames(data[l])," influences the purchase of ",colnames(data[k])," when placed on the same aisle is"))
      print(paste(sop[4]*100," %"))
    }
  }
}
}
sink()

