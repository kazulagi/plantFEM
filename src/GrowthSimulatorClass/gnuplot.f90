module gnuplot
  implicit none
  contains
!===============================================
!Gnuplot形式で結果を出力
!--------------------------
   subroutine gnuplot_out(elem_nod,nod_coord,step,day,day_ID)
   real(8), intent(in) :: nod_coord(:,:)
   integer,intent(in)::elem_nod(:,:),step,day(:,:),day_ID
   integer i,j,yy,mm,dd
   character filename1*17
   character filename2*17
   character filename3*17
   character filename4*17
   !======gnuplot動画出力用コマンド===========
   !データ作成用プログラム
   !連番ファイルの作成-----------------------
   i = step
   yy=day(day_ID,1)
   mm=day(day_ID,2)
   dd=day(day_ID,3)
   write (filename1, '("gnu_out", i6.6, ".txt")') i ! ここでファイル名を生成している
   write (filename3, '("gnu_pet", i6.6, ".txt")') i 
   write (filename4, '("gnu_lef", i6.6, ".txt")') i 
   write (filename2, '("gnu_out", i6.6, ".png")') i
   open(40,file=filename1)
   !----------------------------------------
   
   
   !png出力用スクリプトファイル-----------------
	if(step==1)then
		open(50,file='png_script.gp')
		write(50,*)'set nokey'
		write(50,*)'set  xr[-50:50]'
		write(50,*)'set  yr[-50:50]'
		!write(50,*)'set  zr[0:100]'
		write(50,*)'set view 82,60,1,1'
	else
		open(50,file='png_script.gp',position='append')
	endif
	
	
	write(50,*)'splot ','"',filename1,'" with lines lc rgb "brown" lw 4  '
	write(50,*)'replot ','"',filename3,'"  with lines lc rgb "#00873C" lw 2'
	write(50,*)'replot ','"',filename4,'" with lines lc rgb "#009945" '
	write(50,*)'set label "',yy,'/',mm,'/',dd,'"'
	write(50,*)'set terminal png'
	write(50,*)'set term png size 1024,1024 '
	write(50,*)'set output ','"',filename2,'"'
	write(50,*)'replot'
	write(50,*)'unset label'
	write(50,*)'replot'
	!=============================================

	do i =1, size(elem_nod,1)
			
		write(40,*) nod_coord(elem_nod(i,1),1:3)
		write(40,*) nod_coord(elem_nod(i,2),1:3)
	    write(40,*) '    '
		write(40,*) '    '
	enddo
	
	close(40)
	close(50)

   end subroutine gnuplot_out
end module gnuplot