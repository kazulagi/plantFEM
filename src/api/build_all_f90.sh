plantfem load maize_creator.f90 && plantfem build && cp server.out server_maize_creator.out
plantfem load server_bridge_creator.f90 && plantfem build && cp server.out server_bridge_creator.out
plantfem load server_dam_creator.f90 && plantfem build && cp server.out server_dam_creator.out
plantfem load server_culvert_creator.f90 && plantfem build && cp server.out server_culvert_creator.out
plantfem load soybean_creator.f90 && plantfem build && cp server.out server_soybean_creator.out
plantfem load server_modal_analysis.f90 && plantfem build && cp server.out server_modal_analysis.out
plantfem load server_static_analysis.f90 && plantfem build && cp server.out server_static_analysis.out