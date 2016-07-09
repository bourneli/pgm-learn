#####################################################
# 教材90页的学生例子
#####################################################

p_I <- function(i) {
  if(0 == i) 0.7
  else if(1 == i) 0.3
  else -1
}

p_D <- function(d) {
  if(0 == d) 0.6
  else if(1 == d) 0.4
  else -1
}


p_S_with_I <- function(i,s) {
  t <- data.frame(i = c(0,0,1,1),
                  s = c(0,1,0,1),
                  p = c(.95,.05,.2,.8))
  t[t$i==i & t$s==s,"p"]
}


p_L_with_G <- function(g,l) {
  t <- data.frame(g = c(1,1,2,2,3,3),
                  l = c(0,1,0,1,0,1),
                  p = c(.1,.9,.4,.6,.99,.01))
  t[t$g==g & t$l == l,"p"]
}


p_G_with_I_D <- function(i,d,g) {
  t <- data.frame(i = c(0,0,0,0,0,0,
                        1,1,1,1,1,1),
                  d = c(0,0,0,
                        1,1,1,
                        0,0,0,
                        1,1,1),
                  g = c(1,2,3,
                        1,2,3,
                        1,2,3,
                        1,2,3),
                  p = c(.3,.4,.3,
                        .05,.25,.7,
                        .9,.08,.02,
                        .5,.3,.2))
  t[t$i==i & t$d == d & t$g == g,"p"]
  
}

# 联合概率分布，参见教材54页
p_stu <- function(i,d,g,s,l) {
  
  p_I(i)*p_D(d)*p_G_with_I_D(i,d,g)*p_S_with_I(i,s)*p_L_with_G(g,l)
  
}


# 边缘分布，用于聚合某些条件
stu_p <- function(i_list = c(0,1),
              d_list=c(0,1),
              g_list=c(1,2,3),
              s_list=c(0,1),
              l_list=c(0,1)) {
  prob <- 0
  for(i in i_list){
    for(d in d_list){
      for(g in g_list){
        for(s in s_list){
          for(l in l_list){
            prob <- prob + p_stu(i,d,g,s,l)
          }
        }
      }
    }
  }
  
  prob
}

# 下面3.2.1.2节中的相关条件概率的计算，手动计算会相当麻烦
stu_p(g_list = 3)
stu_p(l=1,i=0) / stu_p(i=0)
stu_p(l=1, d=0, i = 0) / stu_p(d=0, i = 0)
stu_p(i=1,g=3)/stu_p(g=3)
stu_p(d=1,g=3)/stu_p(g=3)
stu_p(i=1, l=0) / stu_p(l=0)
stu_p(i=1, l=0, g=3) / stu_p(l=0, g=3)
stu_p(i=1,  g=3) / stu_p( g=3)
stu_p(i=1, g=3, s=1) / stu_p(g=3, s=1)
stu_p(i=1, g=3, s=1, l=0) / stu_p(g=3, s=1, l = 0)
stu_p(d=1,g=3, s=1)/stu_p(g=3, s=1)


# 一旦知道的父节点的概率，其他的直接或间接连接父节点的概率对当前概率无影响，
# 但是子孙节点的概率会有影响。下面就是例子。
stu_p(g=1,i=1,d=1)/stu_p(i=1,d=1)
stu_p(g=1,i=1,d=1,l=1)/stu_p(i=1,d=1,l=1)
stu_p(g=1,i=1,d=1,s=1)/stu_p(i=1,d=1,s=1)

