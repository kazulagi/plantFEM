
      
    // ページの読み込みを待つ
    //console.log("check 1");
    window.addEventListener('load', init);

        window.onload = function(){
          document.body.oncopy = function(){return false;}
        }
        

        var url = location.href
        function getParam(name, url) {
            if (!url) url = window.location.href;
            name = name.replace(/[\[\]]/g, "\\$&");
            var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
                results = regex.exec(url);
            if (!results) return null;
            if (!results[2]) return '';
            return decodeURIComponent(results[2].replace(/\+/g, " "));
        }

        var leaf_path = getParam("leaf_path",url)
        if(leaf_path==null){
            var leaf_path= window.prompt("File-name of soybean (stl-formatted","soy_leaf.stl");
        };

        var stem_path = getParam("stem_path",url)
        if(stem_path==null){
            var stem_path= window.prompt("File-name of soybean (stl-formatted","soy_stem.stl");
        };

        var root_path = getParam("root_path",url)
        if(root_path==null){
            var root_path= window.prompt("File-name of soybean (stl-formatted","soy_root.stl");
        };
        //var intra_row = getParam("intra_row",url)
        //if(intra_row==null){
        //    var intra_row= window.prompt("How much intra-row spacing (m)?",0.30);
        //};
        //var pl = getParam("pl",url)
        //if(pl==null){
        //    var pl= window.prompt("How many soybean do you plant in a row?",2);
        //};
        //var row = getParam("row",url)
        //if(row==null){
        //    var row= window.prompt("How many rows do you set?",2);
        //};
        //var stage = getParam("stage",url)
        //if(stage==null){
        //    var stage= window.prompt("which growth stage?","R1");
        //};


        
        
    
        
        
        
        function getUniqueStr(myStrong){
            var strong = 1000;
            if (myStrong) strong = myStrong;
            return new Date().getTime().toString(16)  + Math.floor(Math.random()).toString(16)
        }
        document.oncontextmenu = function () {return false;}
        function init() {
            // create botton
            
            var stats = new Stats();
            stats.showPanel(0);
              // Align top-left
            stats.domElement.style.position = 'absolute';
            stats.domElement.style.left = '0px';
            stats.domElement.style.top = '0px';
            
            document.body.appendChild( stats.dom );


            var charts = new Charts();
            charts.showPanel(0);
              // Align top-left
            charts.domElement.style.position = 'absolute';
            charts.domElement.style.left = '0px';
            charts.domElement.style.top = '60px';


            var charts2 = new Biomass();
            charts2.showPanel(0);
              // Align top-left
            charts2.domElement.style.position = 'absolute';
            charts2.domElement.style.left = '0px';
            charts2.domElement.style.top = '120px';


            
            document.body.appendChild( charts.dom );
            document.body.appendChild( charts2.dom );


            THREE.Cache.enabled = false;
            var joystick_right = new VirtualJoystick({
                mouseSupport: true,
                stationaryBase: true,
                baseX: window.innerWidth - 100,
                baseY: window.innerHeight - 100,
                limitStickTravel: true,
                stickRadius: 50
             });
             joystick_right.addEventListener('touchStartValidation', function(event){
                var touch	= event.changedTouches[0];
                event.preventDefault();
                if( touch.pageY < window.innerHeight/2   )	return false;
    
                if( touch.pageX < window.innerWidth-150 )	return false;
    
                return true;
            });
//    
    
            // create controller
            var joystick_left = new VirtualJoystick({
                mouseSupport: true,
                stationaryBase: true,
                baseX: 100,
                baseY: window.innerHeight - 100,
                limitStickTravel: true,
                stickRadius: 50
             });
             joystick_left.addEventListener('touchStartValidation', function(event){
                var touch	= event.changedTouches[0];
                event.preventDefault();
                
                if( touch.pageY < window.innerHeight/2   )	return false;
                if( touch.pageX >= 150 )	return false;

                return true;
            });
    
        var matloader = new THREE.TextureLoader();

        
        var leaf_material = new THREE.MeshLambertMaterial({
          map: matloader.load('https://plantfem.org/image877.png'),
        });
        var root_material = new THREE.MeshLambertMaterial({color: 0xF3E495});
        var stem_material = new THREE.MeshLambertMaterial({color: 0xCBC547});
        var matloader = new THREE.TextureLoader();
        
        let grass_material = new THREE.MeshPhongMaterial();
        var textuerLoader_g = new THREE.TextureLoader();
        let textuer_g = textuerLoader_g.load('https://plantfem.org/grass.jpg');
        grass_material.map = textuer_g;
        grass_material.map.wrapS = THREE.RepeatWrapping;
        grass_material.map.wrapT = THREE.RepeatWrapping;
        grass_material.map.repeat.set(3000, 2500);

        //var grass_material = new THREE.MeshLambertMaterial({color: 0xcccccc});
    
        var tgc_material = new THREE.MeshBasicMaterial({
            //map: texture, // テクスチャーを指定
            color: 0xa9ceec, // 色
            transparent: true, // 透明の表示許可
            blending: THREE.AdditiveBlending, // ブレンドモード
            side: THREE.DoubleSide, // 表裏の表示設定
            depthWrite: false // デプスバッファへの書き込み可否
        });
        
        // create control pannel by dat.GUI
        var controls = new function(){
            this.light_x = 3.00;
            this.light_y = 3.00;
            this.light_z = 80.00;
            this.strength = 15.0;
            this.ground_level = 0.00;
            this.look_up = 0;
            this.look_down = 0;
            this.Soybean = 0;
            //this.inter_row = 0.80;
            //this.intra_row = 0.30;
        }

        var options = {
            go: function() {
              
            location.href='https://plantfem.org/login.html'

            }
          };
        var gui = new dat.GUI();
        var Crop = gui.addFolder('Plant info');
        Crop.add(controls, 'Soybean', 0, 440);
        var Field = gui.addFolder('Environment info');
        Field.add(controls, 'light_x', -10.0, 10.0);
        Field.add(controls, 'light_y', -10.0, 10.0);
        Field.add(controls, 'light_z', 10.0, 1000.0);
        Field.add(controls, 'strength', 0, 30.0);
        Field.add(controls, 'ground_level', -2, 0);
        var Drone = gui.addFolder('Camera info');
        Drone.add(controls, 'look_down', 0, 100);
        Drone.add(controls, 'look_up', 0, 100);
        var YourField = gui.addFolder('Your Field');
        YourField.add(options, 'go')


  
        var renderer = new THREE.WebGLRenderer();
        
        renderer.setSize( window.innerWidth, window.innerHeight );
        renderer.setClearColor(new THREE.Color(0xEEEEEE) );
        //renderer.setClearColorHex( 0x95F3F2, 1 );
        renderer.shadowMap.enabled =  true;
        document.body.appendChild( renderer.domElement );
        var scene = new THREE.Scene();
        var camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );
        camera.position.set(0, -3, 5 );
        const tloader = new THREE.TextureLoader();
        scene.background = tloader.load( 'https://plantfem.org/image.jpg' );
        //var control = new THREE.OrbitControls( camera, renderer.domElement );
        //camera.position.x = camera.position.x - 10.0 
    
        // Objects
        // Soilを作成
    
    
        // Soil
        var soil = new THREE.PlaneGeometry(83, 83);
        

        let textuerLoader = new THREE.TextureLoader();
        let mat = new THREE.MeshPhongMaterial();

        let textuer = textuerLoader.load('https://plantfem.org/soil.jpg');
        mat.map = textuer;
        mat.map.wrapS = THREE.RepeatWrapping;
        mat.map.wrapT = THREE.RepeatWrapping;
        mat.map.repeat.set(30, 25);

        let mat2 = new THREE.MeshPhongMaterial();
        mat2.map = textuer;
        mat2.map.wrapS = THREE.RepeatWrapping;
        mat2.map.wrapT = THREE.RepeatWrapping;
        mat2.map.repeat.set(12, 30);


        //// メッシュを作成
        var soilmesh = new THREE.Mesh(soil, mat);
        soilmesh.position.y=0;
        soilmesh.castShadow = true;
        soilmesh.receiveShadow = true;
        scene.add(soilmesh);
    
        // Soil

        let mat3 = new THREE.MeshPhongMaterial();
        mat3.map = textuer;
        mat3.map.wrapS = THREE.RepeatWrapping;
        mat3.map.wrapT = THREE.RepeatWrapping;
        mat3.map.repeat.set(15, 15);
        var soil = new THREE.PlaneGeometry(12, 30);
        //// メッシュを作成
        var soil1mesh = new THREE.Mesh(soil, mat3);
        soil1mesh.position.y=-8;
        soil1mesh.position.z=0.01;
        soil1mesh.position.x=-8;
        soil1mesh.castShadow = true;
        soil1mesh.receiveShadow = true;
        //scene.add(soil1mesh);
    
        // Soil

        let mat5 = new THREE.MeshPhongMaterial();
        mat5.map = textuer;
        mat5.map.wrapS = THREE.RepeatWrapping;
        mat5.map.wrapT = THREE.RepeatWrapping;
        mat5.map.repeat.set(10, 15);
        var soil1 = new THREE.PlaneGeometry(18, 30);
        //// メッシュを作成
        var soil1mesh = new THREE.Mesh(soil1, mat5);
        soil1mesh.position.y=-8;
        soil1mesh.position.z=0.01;
        soil1mesh.position.x=12;
        soil1mesh.castShadow = true;
        soil1mesh.receiveShadow = true;
        //scene.add(soil1mesh);
    
    
        // Soil

        let mat4 = new THREE.MeshPhongMaterial();
        mat4.map = textuer;
        mat4.map.wrapS = THREE.RepeatWrapping;
        mat4.map.wrapT = THREE.RepeatWrapping;
        mat4.map.repeat.set(32, 11);
        var soil2 = new THREE.PlaneGeometry(32, 11);
        //// メッシュを作成
        var soil2mesh = new THREE.Mesh(soil2, mat4);
        soil2mesh.position.y=16;
        soil2mesh.position.z=0.01;
        soil2mesh.position.x=-4;
        soil2mesh.castShadow = true;
        soil2mesh.receiveShadow = true;
        //scene.add(soil2mesh);
    
        var grass = new THREE.PlaneGeometry(8300, 5000);
        //// メッシュを作成
        var grassmesh = new THREE.Mesh(grass, grass_material);
        grassmesh.position.z=-0.01;
        grassmesh.castShadow = true;
        grassmesh.receiveShadow = true;
        scene.add(grassmesh);
    
    
        // for crop-sci lab
        // Load
        var tgc1 = new THREE.CylinderGeometry(1.6, 1.6, 25.0, 10);
        //// メッシュを作成
        var tgc1mesh = new THREE.Mesh(tgc1, tgc_material);
        tgc1mesh.position.x=-20;
        tgc1mesh.position.y=-6;
        tgc1mesh.castShadow = true;
        tgc1mesh.receiveShadow = true;
        scene.add(tgc1mesh);
        
        var tgc2 = new THREE.CylinderGeometry(1.6, 1.6, 25.0, 10);
        //// メッシュを作成
        var tgc2mesh = new THREE.Mesh(tgc2, tgc_material);
        tgc2mesh.position.x=-25;
        tgc2mesh.position.y=-6;
        tgc2mesh.castShadow = true;
        tgc2mesh.receiveShadow = true;
        scene.add(tgc2mesh);
    
        var tgc3 = new THREE.CylinderGeometry(1.6, 1.6, 25.0, 10);
        //// メッシュを作成
        var tgc3mesh = new THREE.Mesh(tgc3, tgc_material);
        tgc3mesh.position.x=-30;
        tgc3mesh.position.y=-6;
        tgc3mesh.castShadow = true;
        tgc3mesh.receiveShadow = true;
        scene.add(tgc3mesh);
    
    
        var tgc4 = new THREE.CylinderGeometry(1.6, 1.6, 25.0, 10);
        //// メッシュを作成
        var tgc4mesh = new THREE.Mesh(tgc4, tgc_material);
        tgc4mesh.position.x=-35
        tgc4mesh.position.y=-6
        tgc4mesh.castShadow = true;
        tgc4mesh.receiveShadow = true;
        scene.add(tgc4mesh);
    
    
        var greenhouse = new THREE.CylinderGeometry(6.5, 6.5, 13, 10);
        //// メッシュを作成
        var greenhousemesh = new THREE.Mesh(greenhouse, tgc_material);
        greenhousemesh.position.x=32
        greenhousemesh.position.y=0
        greenhousemesh.castShadow = true;
        greenhousemesh.receiveShadow = true;
        scene.add(greenhousemesh);
    
        //// 3D空間にメッシュを追加
        //sphere.position.x +=100; 
        //scene.add(boxmesh);
    
//        // 3DS形式のモデルデータを読み込む
        var loader = new THREE.STLLoader();
        var light = new THREE.PointLight(0xFFFFFF, 3)
        light.position.set(2, 2, 30);
        light.castShadow = true;
        scene.add(light);

    
        // create soybean field
        var group = new THREE.Object3D();
    
    
        scene.add(group)
    
        var rot =0;
    
        // load plants

        
                loader.load( stem_path,  function ( geometry1 ) {
                        var stl_geo_stem = new THREE.Mesh( geometry1, stem_material )
                        stl_geo_stem.rotation.x = 0//Math.PI / 180 * 90
                        stl_geo_stem.rotation.y = 0//Math.PI / 180 * 90
                        stl_geo_stem.rotation.z = Math.PI * (i+j+1);
                        stl_geo_stem.position.y = intra_row*(i)-22;
                        stl_geo_stem.position.x = inter_row*(j)+4;
                        stl_geo_stem.castShadow = true;
                        stl_geo_stem.receiveShadow = true;
                        group.add( stl_geo_stem);
                    
                });
                loader.load( leaf_path,  function ( geometry2 ) {
                    
                        var stl_geo_leaf = new THREE.Mesh( geometry2, leaf_material )
                        stl_geo_leaf.rotation.x = 0//Math.PI / 180 * 90
                        stl_geo_leaf.rotation.y = 0//Math.PI / 180 * 90
                        stl_geo_leaf.rotation.z = Math.PI * (i+j+1);
                        stl_geo_leaf.position.y = intra_row*(i)-22;
                        stl_geo_leaf.position.x = inter_row*(j)+4;
                        stl_geo_leaf.castShadow = true;
                        stl_geo_leaf.receiveShadow = true;
                        group.add( stl_geo_leaf);
                    
                });
                loader.load( root_path,  function ( geometry3 ) {
                    
                        var stl_geo_root = new THREE.Mesh( geometry3, root_material )
                        stl_geo_root.rotation.x = 0//Math.PI / 180 * 90
                        stl_geo_root.rotation.y = 0//Math.PI / 180 * 90
                        stl_geo_root.rotation.z = Math.PI * (i+j+1);
                        stl_geo_root.position.y = intra_row*(i)-22;
                        stl_geo_root.position.x = inter_row*(j)+4;
                        stl_geo_root.castShadow = true;
                        stl_geo_root.receiveShadow = true;
                        group.add( stl_geo_root);
                    
                });

    
        var lookat = new THREE.Vector3(0, 0, 0);
    
        var lookat_x = 0.0;
        var lookat_y = 1000.0;
        var lookat_z = 0.0;
        camera.lookAt(new THREE.Vector3(lookat_x,lookat_y,lookat_z));
        
        camera.position.y=-30.0;
        camera.position.x=5.0;
        camera.position.z=0.6;

        var x = getParam("x",url)
        camera.position.x=x;
        if(x==null){
            camera.position.x=5.0;
        };

        var y = getParam("y",url)
        camera.position.y=y;
        if(y==null){
            camera.position.y=-30.0;
        };
        var z = getParam("z",url)
        camera.position.z=z;
        if(z==null){
            camera.position.z=0.6;
        };

        var angle = getParam("angle",url)
        controls.look_down=angle;
        if(angle==null){
            controls.look_down=0;
        };
        camera.rotateX(-0.020*controls.look_down);
        controls.look_down = 0;

        var vx_n = 0.0;
        var vz_n = 0.0;
        var vx = 0.0;
        var vy = 0.0;
        var vz = 0.0;
        var vrot = 0.0;
        var rot=0;
        var nx=0.0;
        var ny=1.0;
    
    
        var axes = new THREE.AxesHelper(25);
        scene.add(axes);
    
        var lookAtVector = new THREE.Vector3(0,0, 0);
    

        tick();
    
          // 毎フレーム時に実行されるループイベントです
        function tick() {
            //var radian = rot * Math.PI / 180;


            stats.begin();
            charts.begin();
            charts2.begin();
            //stats.update();
            charts.update(20,200);
            charts2.update(20,200);

            requestAnimationFrame(tick);
            
            
            // keyboard operation
            // movement - please calibrate these values
            var xSpeed = 0.00005;
            var ySpeed = 0.00005;
    
            camera.getWorldDirection(lookAtVector );
            //console.log(lookAtVector.x,lookAtVector.y  );
    
            document.addEventListener("keydown", onDocumentKeyDown, false);
            function onDocumentKeyDown(event) {
                var keyCode = event.which;
                if (keyCode == 87) {
                    camera.position.z += ySpeed;
                } else if (keyCode == 83) {
                    camera.position.z -= ySpeed;
                } else if (keyCode == 65) {
                    camera.rotateY(xSpeed)/5.0;
                } else if (keyCode == 68) {
                    camera.rotateY(-xSpeed)/5.0;
                } else if (keyCode == 243) {
                    camera.position.set(0, 0, 0);
                } else if (keyCode == 73) {
                    camera.position.x += 2.0*ySpeed*lookAtVector.x;
                    camera.position.y += 2.0*ySpeed*lookAtVector.y;
                } else if (keyCode == 75) {
                    camera.position.x -= 2.0*ySpeed*lookAtVector.x;
                    camera.position.y -= 2.0*ySpeed*lookAtVector.y;
                } else if (keyCode == 74) {
                    camera.position.x -= ySpeed*lookAtVector.y;
                    camera.position.y += ySpeed*lookAtVector.x;
                } else if (keyCode == 76) {
                    camera.position.x += ySpeed*lookAtVector.y;
                    camera.position.y -= ySpeed*lookAtVector.x;
                } else if (keyCode == 82) {
                    camera.position.set(0, -2, 0);
                    camera.lookAt(0,1000,0);
                }
            };
    
    
            vx = joystick_right.deltaX();
            vy = joystick_right.deltaY();
    
            vz = joystick_left.deltaY();
            vrot = joystick_left.deltaX();
    
            if(Math.abs(vx) > 35.0 || Math.abs(vy) > 35.0){
                vx = 0.0;
                vy = 0.0;
            }
            if(Math.abs(vz) > 35.0 || Math.abs(vrot) > 35.0){
                vz = 0.0;
                vrot = 0.0;
            }
    
            if(Math.abs(vz) > Math.abs(vrot)) {
                vrot = 0.0;
            }else{
                vz = 0.0;
            }        
            
            vrot = Math.PI * vrot/180;
            rot +=vrot;
            
            if(Math.abs(vx) > Math.abs(vy)) {
                vy = 0;
                if(vx > 0.0) {
                    camera.position.x += 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.y;
                    camera.position.y -= 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.x;
                } else{
                    camera.position.x -= 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.y;
                    camera.position.y += 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.x;
                }
            }else{
                vx = 0;
                camera.position.x -= 0.0001*vy*Math.abs(vy)*lookAtVector.x;
                camera.position.y -= 0.0001*vy*Math.abs(vy)*lookAtVector.y;
            };
    
            camera.position.z += -0.0001*vz*Math.abs(vz);
            camera.rotateY(-0.02*vrot*Math.abs(vrot));
            // change angle
            camera.rotateX(0.0001*controls.look_up);
            camera.rotateX(-0.0001*controls.look_down);
    
            a = lookAtVector.x;
            b = lookAtVector.y;

    
            if (camera.position.z < 0.5) { 
                camera.position.z = 0.5;
            }
    
            if (camera.position.x > 150.0) { 
                camera.position.x = 150.0;
            }
    
            if (camera.position.x < -150.0) { 
                camera.position.x = -150.0;
            }
    
    
            if (camera.position.y > 150.0) { 
                camera.position.y = 150.0;
            }
    
            if (camera.position.y < -150.0) { 
                camera.position.y = -150.0;
            }
    
            
                    
            renderer.render(scene, camera); // レンダリング
    
            light.position.x = controls.light_x;
            light.position.y = controls.light_y;
            light.position.z = controls.light_z;
            
            light.power = controls.strength;
            soilmesh.position.z = controls.ground_level;
            
            
            grassmesh.position.z = soilmesh.position.z-0.01;
    
            stats.end();
            charts.end();
            charts2.end();
          }
        }