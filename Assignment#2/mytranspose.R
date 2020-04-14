mytranspose <- function(x){
  
  if(is.null(x)){ # x가 null인 경우
    result <- NULL
  }else{ # null이 아닐 때
    is_type <- which(c(is.vector(x),is.matrix(x), is.data.frame(x))>=1) ## data type 확인  
    if(is_type != 1){ # vector형태가 아닌 경우 바로 matrix형태로 변환
      x <- as.matrix(x)
    }else{## vector형태일 경우 matrix 변환 시 1개의 row가 아닌 1개의 col을 가지게 바뀌므로
      ## 이를 변환해주기 위함.
      x <-matrix(x, ncol=length(x))
    }
    y <- matrix(1, nrow=ncol(x), ncol=nrow(x))
    if(ncol(y)==0&nrow(y)==0){ # 0x0 matrix일 경우 이를 그대로 result에 저장
      result <- y
    }else{ # 0x0 matrix가 아닐 경우 해당 알고리즘 진행
      for(i in 1:nrow(x)){ # transpose
        for(j in 1:ncol(x)){
          y[j,i] <- x[i,j]
        }
      }
      ## 원래 구조로 되돌아가는 알고리즘을 추가하였습니다.
      ## 행렬이나 데이터프레임은 상관없으나 벡터일 경우 행렬로 변환하여 Transpose하면 잘 작동하나
      ## 이를 벡터로 다시 변환하면 Transpose와 상관없이 동일한 결과를 Return합니다.
      ## 예를들어 (1,2,3,4)일 경우 (1x4) 형태인데 이를 Transpose할 경우 행렬 형태가 되면 (4x1)이 되는데
      ## 이를 다시 벡터로 변환하면 (1x4) 형태로 다시 돌아갑니다. 따라서 r에서 벡터 데이터 구조로는
      ## (4x1)을 나타낼 수가 없습니다 교수님.
      result <- list(as.vector(y), as.matrix(y), as.data.frame(y))[is_type][[1]] # 원래 구조로 변환
    }
  }
  return(result)
}
