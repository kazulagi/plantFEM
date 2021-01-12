program main
    use TreeClass
    implicit none

    type(Tree_) :: tree
    type(Node_) :: Node1,Node2,Node3,Node4,node5
    
    call tree%Init()
    call Node1%create(Name="node1")
    call Node2%create(parent=Node1,Name="node2")
    call Node3%create(parent=Node2,Name="node3")
    call Node4%create(parent=Node1,Name="node4")
    call Node5%create(parent=Node4,Name="node5")
    
    call tree%add(Node1)
    call tree%add(Node2)
    call tree%add(Node3)
    call tree%add(Node4)
    call tree%add(Node5)
    call tree%cut(Node5)



    call tree%show()

    print *, "Tree is created."
    
end program 