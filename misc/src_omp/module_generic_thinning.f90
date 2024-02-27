MODULE module_generic_thinning

  USE m_unirnk
  
  USE kd_tree, ONLY: init_kd_tree, close_kd_tree, &
       &knn_search_ts, knn_search

  IMPLICIT NONE
  
CONTAINS

  SUBROUTINE thinning_sphere(grid1,grid2,ndata,dphi_max,output)

    REAL, INTENT(in) :: grid1(:,:),grid2(:,:)
    INTEGER, INTENT(in) :: ndata
    REAL, INTENT(in) :: dphi_max
    INTEGER, ALLOCATABLE, INTENT(out) :: output(:)

    REAL :: hp1(3),hp2(3)
    INTEGER :: num_nn,num_nn_found,ndata_thin
    REAL, ALLOCATABLE :: min_d(:)
    INTEGER, ALLOCATABLE :: nn(:),tmp(:),tmp_unique(:)
    INTEGER :: i,j,nxy

    CONTINUE

    hp1=0.
    hp2=0.
!number of closest neighbors

    nxy=SIZE(grid2,1)    

!    PRINT *,MINVAL(grid1(:,1)),MAXVAL(grid1(:,1))
!    PRINT *,MINVAL(grid1(:,2)),MAXVAL(grid1(:,2))
!
!    PRINT *,MINVAL(grid2(:,1)),MAXVAL(grid2(:,1))
!    PRINT *,MINVAL(grid2(:,2)),MAXVAL(grid2(:,2))
!
!
!    PRINT *,SIZE(grid2,1),SIZE(grid2,2)
!    PRINT *,SIZE(grid1,1),SIZE(grid1,2)
!

    num_nn=1

    CALL init_kd_tree(grid1, ndata, num_nn)

    ALLOCATE(nn(num_nn),min_d(num_nn),tmp(nxy))

    tmp=0
    
!       CALL CPU_TIME(start)
    
    j=0
       
!$OMP PARALLEL DO DEFAULT (NONE) &
!$OMP SHARED (grid2,dphi_max,hp1,hp2,tmp,num_nn,nxy,j) &
!$OMP PRIVATE (nn,min_d,num_nn_found,i) 
       
    DO i=1,nxy
!       CALL knn_search(grid2(i,:),nn,min_d,hp1,hp2,1.0,num_nn_found)
       CALL knn_search_ts(grid2(i,1:2),nn,min_d,hp1,hp2,1.0,num_nn,num_nn_found)
       IF ( num_nn_found > 0 .AND. MINVAL(min_d) < dphi_max ) THEN
          
!!$OMP ATOMIC UPDATE          
!$OMP CRITICAL
          j=j+1
          tmp(j)=nn(1)
!$OMP END CRITICAL
             
       ENDIF
    ENDDO
    
!       DO i=1,j
!          WRITE(105,*)tmp(i)
!       ENDDO
    
!       CALL CPU_TIME(finish)
!       PRINT *,'Time spent in kd_search = ',finish-start
       
    CALL close_kd_tree()
       
    ALLOCATE(tmp_unique(j))
    
    CALL unirnk(tmp(1:j),tmp_unique,ndata_thin)
    
!    DO i=1,ndata_thin
!       WRITE(106,*)tmp(tmp_unique(i))
!    ENDDO
    
    ALLOCATE(output(ndata_thin))
    output=tmp(tmp_unique(1:ndata_thin))

    DEALLOCATE(tmp,tmp_unique)

  END SUBROUTINE thinning_sphere

  SUBROUTINE thinning_sphere_nn(grid1,grid2,dphi_max,num_nn,output)

    USE module_constants, ONLY: imissing

    REAL, INTENT(in) :: grid1(:,:),grid2(:,:)
    REAL, INTENT(in) :: dphi_max
    INTEGER, ALLOCATABLE, INTENT(out) :: output(:,:)
    INTEGER, INTENT(in) :: num_nn !number of neighbors

    REAL :: hp1(3),hp2(3)
    INTEGER :: ndata,num_nn_found
    REAL, ALLOCATABLE :: min_d(:)
    INTEGER, ALLOCATABLE :: nn(:)
    INTEGER :: i,j,k,npoints

    REAL :: start,finish

    CONTINUE

    hp1=0.
    hp2=0.

    ndata=SIZE(grid1,1)
    npoints=SIZE(grid2,1)    

!    PRINT *,MINVAL(grid1(:,1)),MAXVAL(grid1(:,1))
!    PRINT *,MINVAL(grid1(:,2)),MAXVAL(grid1(:,2))
!
!    PRINT *,MINVAL(grid2(:,1)),MAXVAL(grid2(:,1))
!    PRINT *,MINVAL(grid2(:,2)),MAXVAL(grid2(:,2))
!
!
!    PRINT *,SIZE(grid1,1),SIZE(grid1,2)
!    PRINT *,SIZE(grid2,1),SIZE(grid2,2)


!    PRINT *,'@@@4.1',ndata,grid2(:,1)
!    PRINT *,'@@@4.2',ndata,grid2(:,2)
!    PRINT *,'@@@5',ndata,MINVAL(grid1(:,1)),MAXVAL(grid1(:,1))


    CALL CPU_TIME(start)

    CALL init_kd_tree(grid1, ndata, num_nn)

!    PRINT *,'@@@6',grid2(:,1)
!    PRINT *,'@@@7',grid2(:,2)
    
    ALLOCATE(nn(num_nn),min_d(num_nn),output(npoints,num_nn))
    
    output=imissing

    DO i=1,npoints

!       PRINT *,'@@@2',grid2(i,1),MINVAL(grid1(:,1)),MAXVAL(grid1(:,1))

       CALL knn_search(grid2(i,1:2),nn,min_d,hp1,hp2,1.0,num_nn_found)

       IF ( num_nn_found > 0 ) THEN
          k=0

!          PRINT *,'@@@3',i,num_nn_found,MINVAL(min_d),MAXVAL(min_d),dphi_max
          
          DO j=1,num_nn_found
             IF (min_d(j) < dphi_max) THEN
!                PRINT *,'@@@3.1',j,min_d(j),dphi_max,k,nn(j)
!                PRINT *,'@@@3.2',grid2(i,1:2)
!                PRINT *,'@@@3.2',grid1(nn(j),1:2)

                k=k+1
                output(i,k)=nn(j)
!                PRINT *,'@@@3.2',i,k,output(i,k)
             ENDIF
          ENDDO

       ENDIF

    ENDDO

    CALL CPU_TIME(finish)
    PRINT *,'Time spent in kd_search = ',finish-start

    CALL close_kd_tree()

  END SUBROUTINE thinning_sphere_nn


END MODULE module_generic_thinning
