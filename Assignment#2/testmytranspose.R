assertEquals <- function(expected, result){
  state <- FALSE # 초기값 설정
  if(is.null(expected) & is.null(result)){ # 두 값 모두 null인 경우
    state <- TRUE
  }else if(is.null(expected) | is.null(result)){ # 두 값 중 하나가 null인 경우
    state <- FALSE
  }else{ # 두 값 모두 null이 아닐 경우
    expected <- as.matrix(expected)
    result <- as.matrix(result)
    if(((ncol(expected)==ncol(result) & (nrow(expected)==nrow(result))))){ # 두 matrix 크기가 같은 경우 
      if((ncol(expected)==0&nrow(expected)==0) & (ncol(result)==0&nrow(result)==0)){ # 모두 0x0 matrix일 경우 True
        state <- TRUE
      }else if((ncol(expected)==0&nrow(expected)==0) | (ncol(result)==0&nrow(result)==0)){ # 둘 중 하나가 0x0 matrix일 경우 False
        state <- FALSE      
      }else{ # 두 matrix 모두 0x0 matrix가 아닐 경우 해당 알고리즘 진행
        
        for(i in 1:nrow(expected)){ 
          for(j in 1:ncol(expected)){
            if(is.na(expected[i,j]) & is.na(result[i,j])){ #두 element가 NA인 경우
              state <- TRUE
            }else if(is.na(expected[i,j]) | is.na(result[i,j])){ #하나의 element가 NA인 경우
              state <- FALSE
              break
            }else{ # 모두 NA가 아닌 경우
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
expected <- c(1,2,NA,3) ## 벡터로는 transpose를 나타낼 수가 없습니다......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar2 <- c(NA)
expected <- c(NA) ## 벡터로는 transpose를 나타낼 수가 없습니다......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar2 <- c()
expected <- c() ## 벡터로는 transpose를 나타낼 수가 없습니다......
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
expected <- c(1,2,NA,3) ## 벡터로는 transpose를 나타낼 수가 없습니다......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar1 <- matrix(1:10, nrow=5, ncol=2)
expected <- matrix(1:12, nrow=2, ncol=6, byrow = TRUE)
result <- mytranspose(myvar1)
assertEquals(expected, result)

myvar2 <- c(NA)
expected <- c() ## 벡터로는 transpose를 나타낼 수가 없습니다......
result <- mytranspose(myvar2)
assertEquals(expected, result)

myvar2 <- c(1)
expected <- c() ## 벡터로는 transpose를 나타낼 수가 없습니다......
result <- mytranspose(myvar2)
assertEquals(expected, result)
