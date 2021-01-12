program main
    use std
    implicit none

    type(Vertex_) :: vertex(10)
    type(Graph_) :: graph

    call vertex(1)%create(Name="Nagoya"     ,intval=100 ,Myrank=0)
    call vertex(2)%create(Name="Osaka"      ,intval=20  ,Myrank=0)
    call vertex(3)%create(Name="Kyoto"      ,intval=60  ,Myrank=0)
    call vertex(4)%create(Name="Tokyo"      ,intval=10  ,Myrank=0)
    call vertex(5)%create(Name="Ishioka"    ,intval=120 ,Myrank=0)
    call vertex(6)%create(Name="Hokkaido"   ,intval=1   ,Myrank=0)
    call vertex(7)%create(Name="Hawaii"     ,intval=20  ,Myrank=0)
    call vertex(8)%create(Name="Heaven"     ,intval=30  ,Myrank=0)
    
    call graph%add(vertex=vertex(1) )
    call graph%add(vertex=vertex(2) )
    call graph%add(vertex=vertex(3) )
    call graph%add(vertex=vertex(4) )
    call graph%add(vertex=vertex(5) )
    call graph%add(vertex=vertex(6) )
    call graph%add(vertex=vertex(7) )
    call graph%add(vertex=vertex(8) )
    
    call graph%add(from=1, to=2)
    call graph%add(from=1, to=5)
    call graph%add(from=4, to=1)
    call graph%add(from=2, to=3)
    call graph%add(from=6, to=2)
    call graph%add(between=1,and=2)
    call graph%add(between=1,and=3)
    call graph%add(between=1,and=4)
    call graph%add(between=1,and=5)
    call graph%add(between=1,and=6)
    call graph%add(between=1,and=7)
    
    call graph%show()
    
end program main