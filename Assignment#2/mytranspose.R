mytranspose <- function(x){
  
  if(is.null(x)){ # x�� null�� ���
    result <- NULL
  }else{ # null�� �ƴ� ��
    is_type <- which(c(is.vector(x),is.matrix(x), is.data.frame(x))>=1) ## data type Ȯ��  
    if(is_type != 1){ # vector���°� �ƴ� ��� �ٷ� matrix���·� ��ȯ
      x <- as.matrix(x)
    }else{## vector������ ��� matrix ��ȯ �� 1���� row�� �ƴ� 1���� col�� ������ �ٲ�Ƿ�
      ## �̸� ��ȯ���ֱ� ����.
      x <-matrix(x, ncol=length(x))
    }
    y <- matrix(1, nrow=ncol(x), ncol=nrow(x))
    if(ncol(y)==0&nrow(y)==0){ # 0x0 matrix�� ��� �̸� �״�� result�� ����
      result <- y
    }else{ # 0x0 matrix�� �ƴ� ��� �ش� �˰����� ����
      for(i in 1:nrow(x)){ # transpose
        for(j in 1:ncol(x)){
          y[j,i] <- x[i,j]
        }
      }
      ## ���� ������ �ǵ��ư��� �˰������� �߰��Ͽ����ϴ�.
      ## ����̳� �������������� ��������� ������ ��� ��ķ� ��ȯ�Ͽ� Transpose�ϸ� �� �۵��ϳ�
      ## �̸� ���ͷ� �ٽ� ��ȯ�ϸ� Transpose�� ������� ������ ����� Return�մϴ�.
      ## ������� (1,2,3,4)�� ��� (1x4) �����ε� �̸� Transpose�� ��� ��� ���°� �Ǹ� (4x1)�� �Ǵµ�
      ## �̸� �ٽ� ���ͷ� ��ȯ�ϸ� (1x4) ���·� �ٽ� ���ư��ϴ�. ���� r���� ���� ������ �����δ�
      ## (4x1)�� ��Ÿ�� ���� �����ϴ� ������.
      result <- list(as.vector(y), as.matrix(y), as.data.frame(y))[is_type][[1]] # ���� ������ ��ȯ
    }
  }
  return(result)
}