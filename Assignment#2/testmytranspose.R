assertEquals <- function(expected, result){
  state <- FALSE # �ʱⰪ ����
  if(is.null(expected) & is.null(result)){ # �� �� ��� null�� ���
    state <- TRUE
  }else if(is.null(expected) | is.null(result)){ # �� �� �� �ϳ��� null�� ���
    state <- FALSE
  }else{ # �� �� ��� null�� �ƴ� ���
    expected <- as.matrix(expected)
    result <- as.matrix(result)
    if(((ncol(expected)==ncol(result) & (nrow(expected)==nrow(result))))){ # �� matrix ũ�Ⱑ ���� ��� 
      if((ncol(expected)==0&nrow(expected)==0) & (ncol(result)==0&nrow(result)==0)){ # ��� 0x0 matrix�� ��� True
        state <- TRUE
      }else if((ncol(expected)==0&nrow(expected)==0) | (ncol(result)==0&nrow(result)==0)){ # �� �� �ϳ��� 0x0 matrix�� ��� False
        state <- FALSE      
      }else{ # �� matrix ��� 0x0 matrix�� �ƴ� ��� �ش� �˰����� ����
        
        for(i in 1:nrow(expected)){ 
          for(j in 1:ncol(expected)){
            if(is.na(expected[i,j]) & is.na(result[i,j])){ #�� element�� NA�� ���
              state <- TRUE
            }else if(is.na(expected[i,j]) | is.na(result[i,j])){ #�ϳ��� element�� NA�� ���
              state <- FALSE
              break
            }else{ # ��� NA�� �ƴ� ���
              if(expected[i,j]==result[i,j]){
                state <- TRUE
              }else{
                state <- FALSE
                break
              }
            }
          }
          if(!state){
            break
          }
        }
      }
    }else{
      state = FALSE
    }
  }
  return(state)
}

source('mytranspose.R')

myvar1 <- matrix(1:10, nrow=5, ncol=2)
expected <- matrix(1:10, nrow=2, ncol=5, byrow = TRUE)
result <- mytranspose(myvar1)
assertEquals(expected, result)

myvar1 <- matrix(NA, nrow=0, ncol=0)
expected <- matrix(NA, nrow=0, ncol=0)
result <- mytranspose(myvar1)
assertEquals(expected, result)

myvar1 <- matrix(c(1,2), nrow=1, ncol=2)
expected <- matrix(c(1,2), nrow=2, ncol=1)
result <- mytranspose(myvar1)
assertEquals(expected, result)

myvar1 <- matrix(c(1,2), nrow=2, ncol=1)
expected <- matrix(c(1,2), nrow=1, ncol=2)
result <- mytranspose(myvar1)
assertEquals(expected, result)

myvar2 <- c(1,2,NA,3)
expected <- c(1,2,NA,3) ## ���ͷδ� transpose�� ��Ÿ�� ���� �����ϴ�......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar2 <- c(NA)
expected <- c(NA) ## ���ͷδ� transpose�� ��Ÿ�� ���� �����ϴ�......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar2 <- c()
expected <- c() ## ���ͷδ� transpose�� ��Ÿ�� ���� �����ϴ�......
result <- mytranspose(myvar2)
assertEquals(expected, result)

d <- c(1,2,3,4)
e <- c("red", "white", "red", "NA")
f <- c(TRUE,TRUE,TRUE,FALSE)
myvar3 <- data.frame(d,e,f)
expected <- as.data.frame(t(myvar3))
result <- mytranspose(myvar3)
assertEquals(expected, result)

###################### FALSE case #############################

myvar2 <- c(1,2,3,3)
expected <- c(1,2,NA,3) ## ���ͷδ� transpose�� ��Ÿ�� ���� �����ϴ�......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar1 <- matrix(1:10, nrow=5, ncol=2)
expected <- matrix(1:12, nrow=2, ncol=6, byrow = TRUE)
result <- mytranspose(myvar1)
assertEquals(expected, result)

myvar2 <- c(NA)
expected <- c() ## ���ͷδ� transpose�� ��Ÿ�� ���� �����ϴ�......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar2 <- c(1)
expected <- c() ## ���ͷδ� transpose�� ��Ÿ�� ���� �����ϴ�......
result <- mytranspose(myvar2)
assertEquals(expected, result)