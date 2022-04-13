program MFA_project


implicit none	

!This project is created for personal requirements you may change the NrowXX which is and NcolXX which is the column number row number of our matrix.
integer::NrowXX= (7+1)*19, NcolXX=(7+1)*23
real,dimension(152,184):: matXX, avematXX
real :: kXX=0, cumulative_sum=0,sum=0,average=0,averageXX=0
integer:: i,j,e

    
!!matXX
print*, "----DEFINING THE INPUTS OF MATXX---"
do i=1,NrowXX
   do j=1,NcolXX
     do e=0,i+j
          kXX = e
          !!Sum is reaching nearly 3.15 because each process it is getting much less than 0.
          sum = (8.0/((4*kXX+1) * (4*kXX+3))) !!This can be also changed. You may put an equation whatever you want.
          cumulative_sum = cumulative_sum + sum
          matXX(i,j) = cumulative_sum     
      end do
      !!Reset all
      cumulative_sum = 0
      sum = 0
     
     print*,i,j,matXX(i,j)
     
	end do
end do
    
!!aveMat
!!I am checking all the conditions of upper/lower top/bottom corner or centric
print*, "----FOR CALCULATING AVERAGE OF MATXX---"
do i=1,NrowXX
   do j=1,NcolXX
     
     if (i==1) THEN !!On Top Row

     	if(j==1) THEN !!On top left corner
        	average = (matXX(i,j) + matXX(i+1,j) + matXX(i,j+1))/3.0
  			print*, "UPPER LEFT CORNER"," row:",i,"column:",j
            
     	else if(j==NcolXX) THEN !!On top right corner
        	average = (matXX(i,j) + matXX(i+1,j) + matXX(i,j-1))/3.0
            print*, "UPPER RIGHT CORNER"," row:",i,"column:",j
        else         
          	average = (matXX(i,j) + matXX(i+1,j) + matXX(i,j+1) + matXX(i,j-1))/4.0  	
            print*, "UPPER ROW"," row:",i,"column:",j
            
        	end if

     	else if (i==NrowXX) THEN !!On Bottom Row
       
      	if(j==1) THEN !!On bottom left corner
             average = (matXX(i,j) + matXX(i-1,j) + matXX(i,j+1))/3.0
             print*, "BOTTOM LEFT CORNER"," row:",i,"column:",j
	
	  	else if(j==NcolXX) THEN !!On bottom right corner
			 average = (matXX(i,j) + matXX(i-1,j) + matXX(i,j-1))/3.0
              print*, "BOTTOM RIGHT CORNER"," row:",i,"column:",j
		else         
          	average = (matXX(i,j) + matXX(i-1,j) + matXX(i,j+1) + matXX(i,j-1))/4.0  	
            print*, "BOTTOM ROW"," row:",i,"column:",j
              
             end if
                
      else if (j==1 .and. (i /= 1 .or. (i /= NrowXX))) THEN !!On Left Column and not in the corners
       	 	 average = (matXX(i,j) + matXX(i-1,j) + matXX(i+1,j) + matXX(i,j+1))/4.0
              print*, "LEFT COLUMN"," row:",i,"column:",j
        
	  else if (j==NcolXX .and. (i /= 1 .or. (i /= NrowXX))) THEN !!On Right Column and not in the corners
      		 average = (matXX(i,j) + matXX(i-1,j) + matXX(i+1,j) + matXX(i,j-1))/4.0
              print*, "RIGHT COLUMN"," row:",i,"column:",j

      else if ((j /=1 .or. j/=(NcolXX)) .and. (i /= 1 .or. (i /= NrowXX))) THEN !!On any of the centric place
       	     average = (matXX(i,j) + matXX(i-1,j) + matXX(i+1,j) + matXX(i,j-1) + matXX(i,j+1))/5.0
              print*, "CENTRIC"," row:",i,"column:",j
      else
           print*, "UNIDENTIFIED (FOR DEBUGGING)"

      end if
      
   avematXX(i,j)= average
   print*, (avematXX(i,j))
   average = 0
   end do

end do

average = 0  

print*, "----FOR PRINTING THE LAST AVERAGE DATA----"
print*, "----LATEST DATAS----"


do i=1,NrowXX
   do j=1,NcolXX
  
     sum = avematXX(i,j)
     cumulative_sum =  cumulative_sum + sum
     !!FOR SHOWING AverageXX which is needed at the and of the project folder.  
	end do
end do

print*, (cumulative_sum/(NrowXX*NcolXX))

!!Summary: As dimension of matrices increases average shifts its final value which is nearly 3.14
!!As dimension of matrices decreases average shifts its initial value which is nearly 2.9
!!If dimensions are square shaped like N x N average is roughly (final value-initial value) / 2

end program
