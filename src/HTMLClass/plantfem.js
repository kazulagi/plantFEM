
        // ページの読み込みを待つ
        
        window.addEventListener('load', init);
 
        window.onload = function(){
          document.body.oncopy = function(){return false;}
        }
        
        const canvas = document.querySelector('#c');
        //document.write('<center>');
        //document.write('<span style="font-family:Courier">[w:up] [s:down] [a-d:rotate] [i:go] [k:back] [j:left] [l:right]</span>');
        
        var inter_row= window.prompt("How much inter-row spacing (m)?",0.780);
        var intra_row= window.prompt("How much intra-row spacing (m)?",0.30);
    
        var pl= window.prompt("How many soybean do you plant in a row?",2);
        var row= window.prompt("How many rows do you set?",2);
    /*
    * タッチ操作での拡大縮小禁止
    */
    function no_scaling() {
        document.addEventListener("touchmove", mobile_no_scroll, { passive: false });
    }
    
    /**
    * タッチ操作での拡大縮小禁止解除
    */
    function return_scaling() {
        document.removeEventListener('touchmove', mobile_no_scroll, { passive: false });
    }
    
    /**
    * 拡大縮小禁止
    */
    function mobile_no_scroll(event) {
        // ２本指での操作の場合
        if (event.touches.length >= 2) {
            // デフォルトの動作をさせない
            event.preventDefault();
        }
    }
    
             //スクロールを禁止する関数
            function noScroll(event) {
              event.preventDefault();
            }
            //スクロール禁止(SP)
           document.addEventListener('touchmove', noScroll, { passive: false });
            //スクロール禁止(PC)
           document.addEventListener('mousewheel', noScroll, { passive: false });
          
        /**
        * 拡大縮小禁止
        */
        function mobile_no_scroll(event) {
            // ２本指での操作の場合
            if (event.touches.length >= 2) {
                // デフォルトの動作をさせない
                event.preventDefault();
            }
        }
    
        noScroll();
        mobile_no_scroll();
    
        document.oncontextmenu = function () {return false;}
    //    window.onload = function() {
    //        var canvas = document.getElementById('canvas');  // HTMLCanvasElement
    //        var ctx = canvas.getContext('2d');  // CanvasRenderingContext2D
    //        ctx.font = '50pt Arial';
    //        ctx.fillStyle = 'rgba(0, 0, 255)';
    //        ctx.fillText('Hello', 20, 70);  // 座標 (20, 50) にテキスト描画
    //    };
    //
    
    console.log(window.innerHeight,window.innerWidth)
        function init() {
            THREE.Cache.enabled = false;
            //
        
            console.log(window.innerHeight,window.innerWidth)
    
            // create controller
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
            //var joystick_right = new VirtualJoystick({
            //    mouseSupport: true,
            //    stationaryBase: true,
            //    baseX: 1000,
            //    baseY: 100,
            //    limitStickTravel: true,
            //    stickRadius: 50
            // });
            // get inter- and intra- row size
            // 
    
        
        //  // サイズを指定
        //  var width = 960;
        //  var height = 540;
    //
        //  // レンダラーを作成
        //  var renderer = new THREE.WebGLRenderer({
        //    canvas: document.querySelector('#myCanvas')
        //  });
        //  renderer.setPixelRatio(window.devicePixelRatio);
        //  renderer.setSize(width, height);
    //
        //// シーンを作成
        //var scene = new THREE.Scene();
    
        // materials
        
    //    var leaf_material = new THREE.MeshBasicMaterial({
    //        //map: texture, // テクスチャーを指定
    //        color: 0x008000, // 色
    //        transparent: false, // 透明の表示許可
    //        blending: THREE.AdditiveBlending, // ブレンドモード
    //        side: THREE.DoubleSide, // 表裏の表示設定
    //        depthWrite: false // デプスバッファへの書き込み可否
    //    });
    //    var root_material = new THREE.MeshBasicMaterial({
    //        //map: texture, // テクスチャーを指定
    //        color: 0x007eff, // 色
    //        transparent: false, // 透明の表示許可
    //        blending: THREE.AdditiveBlending, // ブレンドモード
    //        side: THREE.DoubleSide, // 表裏の表示設定
    //        depthWrite: false // デプスバッファへの書き込み可否
    //    });
    //    var stem_material = new THREE.MeshBasicMaterial({
    //        //map: texture, // テクスチャーを指定
    //        color: 0x0CBC547, // 色
    //        transparent: false, // 透明の表示許可
    //        //blending: THREE.AdditiveBlending, // ブレンドモード
    //        //side: THREE.DoubleSide, // 表裏の表示設定
    //        //depthWrite: false // デプスバッファへの書き込み可否
    //    });
    //    var soil_material = new THREE.MeshBasicMaterial({
    //        //map: texture, // テクスチャーを指定
    //        color: 0x0542D24, // 色
    //        transparent: false, // 透明の表示許可
    //        blending: THREE.AdditiveBlending, // ブレンドモード
    //        side: THREE.DoubleSide, // 表裏の表示設定
    //        depthWrite: false // デプスバッファへの書き込み可否
    //    });
    
        // stat info
        //function initStats(){
        //    var stats = new Stats();
        //    stats.setMode(0);
        //    stats.domElement.style.position = 'absolute';
        //    stats.domElement.style.left = '0px';
        //    stats.domElement.style.top = '0px';
        //    document.getElementById("Stats-output").appendChild(
        //        stats.domElement);
        //    return stats;
        //}
        var stats = new Stats();
        stats.setMode(0);
        stats.domElement.style.left = '0px';
        stats.domElement.style.top = '0px';
        //document.getElementById("Stats-output").appendChild(stats.domElement);
    
        var matloader = new THREE.TextureLoader();

        
        var leaf_material = new THREE.MeshLambertMaterial({
          map: matloader.load('image877.png'),
        });
//        let leaf_material = textuerLoader_leaf.load('image877.png');
//        leaf_material.map = leaf_material;
//        leaf_material.map.wrapS = THREE.RepeatWrapping;
//        leaf_material.map.wrapT = THREE.RepeatWrapping;
//        leaf_material.map.repeat.set(3, 3);
//        //var leaf_material = new THREE.MeshLambertMaterial({color: 0x005E15});
        
        var root_material = new THREE.MeshLambertMaterial({color: 0xF3E495});
        var stem_material = new THREE.MeshLambertMaterial({color: 0xCBC547});
        var matloader = new THREE.TextureLoader();

//        var soil_material = new THREE.MeshLambertMaterial({
//            map: matloader.load('soil.jpg'),
//          });
//          // scale x2 proportional
//        //soil_material.repeat.set(0.5, 0.5);
//
//        var soil1_material = new THREE.MeshLambertMaterial({
//          map: matloader.load('soil.jpg'),
//        });
        // scale x2 proportional
        //soil1_material.repeat.set(0.5, 0.5);

        //var grass_material = new THREE.MeshLambertMaterial({color: 0x542D24});

        
        let grass_material = new THREE.MeshPhongMaterial();
        var textuerLoader_g = new THREE.TextureLoader();
        let textuer_g = textuerLoader_g.load('grass.jpg');
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
        
//        var soil1_material = new THREE.MeshBasicMaterial({
//            //map: texture, // テクスチャーを指定
//            color: 0x542D24, // 色
//            transparent: false, // 透明の表示許可
//            blending: THREE.AdditiveBlending, // ブレンドモード
//            side: THREE.DoubleSide, // 表裏の表示設定
//            depthWrite: false // デプスバッファへの書き込み可否
//        });
//    
        // create control pannel by dat.GUI
        var controls = new function(){
            this.light_x = 3.00;
            this.light_y = 3.00;
            this.light_z = 80.00;
            this.strength = 15.0;
            this.ground_level = 0.00;
            //this.inter_row = 0.80;
            //this.intra_row = 0.30;
        }
        var gui = new dat.GUI();
        gui.add(controls, 'light_x', -10.0, 10.0);
        gui.add(controls, 'light_y', -10.0, 10.0);
        gui.add(controls, 'light_z', 10.0, 1000.0);
        gui.add(controls, 'strength', 0, 30.0);
        gui.add(controls, 'ground_level', -1.0, 1.0);
        
        //gui.add(controls, 'inter_row', 0.0, 2.00);
        //gui.add(controls, 'intra_row', 0.0, 2.00);
        
    
        // カメラを作成
        //var camera = new THREE.PerspectiveCamera(45, width / height);
        //camera.position.set(0, 0, 1000);
        // カメラコントローラーを作成
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
        scene.background = tloader.load( 'image.jpg' );
        //var control = new THREE.OrbitControls( camera, renderer.domElement );
        
    
        // Objects
        // Soilを作成
    
    
        // Soil
        var soil = new THREE.PlaneGeometry(83, 83);
        

        let textuerLoader = new THREE.TextureLoader();
        let mat = new THREE.MeshPhongMaterial();

        let textuer = textuerLoader.load('soil.jpg');
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
    
        // 3DS形式のモデルデータを読み込む
        var loader = new THREE.STLLoader();
        // var stl_material_stem = new THREE.MeshLambertMaterial({color: 0xffffff});
        loader.load( './soy_stem.stl', function ( geometry1 ) {
            var stl_geo_stem = new THREE.Mesh( geometry1, stem_material )
            stl_geo_stem.rotation.x = 0//Math.PI / 180 * 90
            stl_geo_stem.rotation.y = 0//Math.PI / 180 * 90
            stl_geo_stem.rotation.z = 0
            stl_geo_stem.castShadow = true;
            stl_geo_stem.receiveShadow = true;
            scene.add( stl_geo_stem );
        });
        // /var stem = new THREE.Mesh( stem_stl, stem_material );
        
        var loader = new THREE.STLLoader();
        //var stl_material_leaf = new THREE.MeshLambertMaterial({color: 0xffffff});
        loader.load( './soy_leaf.stl', function ( geometry2 ) {
            var stl_geo_leaf = new THREE.Mesh( geometry2, leaf_material )
            stl_geo_leaf.rotation.x = 0//Math.PI / 180 * 90
            stl_geo_leaf.rotation.y = 0//Math.PI / 180 * 90
            stl_geo_leaf.rotation.z = 0
            stl_geo_leaf.castShadow = true;
            stl_geo_leaf.receiveShadow = true;
            scene.add( stl_geo_leaf );
        });
    
        var loader = new THREE.STLLoader();
        //var stl_material_root = new THREE.MeshLambertMaterial({color: 0xffffff});
        loader.load( './soy_root.stl', function ( geometry ) {
            var stl_geo_root = new THREE.Mesh( geometry, root_material )
            stl_geo_root.rotation.x = 0//Math.PI / 180 * 90
            stl_geo_root.rotation.y = 0//Math.PI / 180 * 90
            stl_geo_root.rotation.z = 0
            stl_geo_root.castShadow = true;
            stl_geo_root.receiveShadow = true;
            scene.add( stl_geo_root );
        });
    
        /// mouse operation
        // let rot = 0;
        // let mouseX = 0;
    
        // マウス座標はマウスが動いた時のみ取得できる
        //document.addEventListener("mousemove", (event) => {
        //  mouseX = event.pageX;
        //});
    
        // 平行光源を作成
        // new THREE.DirectionalLight(色, 光の強さ)
        //var light = new THREE.DirectionalLight(0xFFFFFF, 1);
        //light.position.set(0, 0, 10)
        //scene.add(light);
    
        //var light = new THREE.SpotLight(0xffffff, 10);
        var light = new THREE.PointLight(0xFFFFFF, 3)
        light.position.set(2, 2, 30);
        light.castShadow = true;
        scene.add(light);

        
        // /var pointlight = new THREE.PointLight(0xFFFFFF, 1);
        // /pointlight.position.set(0, 0, 5)
        // /pointlight.position.z = 10
        // /scene.add(pointlight);
    
        // create soybean field
        var group = new THREE.Object3D();
    
    
        scene.add(group)
    
        var rot =0;
    
        //var inter_row=controls.inter_row;
        //var intra_row=controls.intra_row;
    
        for(let j=0; j<row; j++){
            for (let i=0; i<pl; i++){
                loader.load( './soy_stem.stl', function ( geometry1 ) {
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
                loader.load( './soy_leaf.stl', function ( geometry2 ) {
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
                loader.load( './soy_root.stl', function ( geometry3 ) {
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
            }
        }
        window.alert("Please zoom out and find your soybeans. \n It may takes a few minutes to load a field...");
    
        var lookat = new THREE.Vector3(0, 0, 0);
    
        var lookat_x = 0.0;
        var lookat_y = 1000.0;
        var lookat_z = 0.0;
        camera.lookAt(new THREE.Vector3(lookat_x,lookat_y,lookat_z));
        camera.position.y=-2.0;
        camera.position.z=0.5;
    
        var vx_n = 0.0;
        var vz_n = 0.0;
        var vx = 0.0;
        var vy = 0.0;
        var vz = 0.0;
        var vrot = 0.0;
        var rot=0;
        var nx=0.0;
        var ny=1.0;
    
    
        var ax = 0.0;
        var ay = 0.0;
    
        var count = 0;
    
    
        var axes = new THREE.AxisHelper(25);
        scene.add(axes);
    
        var lookAtVector = new THREE.Vector3(0,0, 0);
    
        tick();
    
          // 毎フレーム時に実行されるループイベントです
          function tick() {
            //var radian = rot * Math.PI / 180;
            stats.update();
    
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
                    camera.position.y -= ySpeed*lookAtVector.x;
                } else if (keyCode == 76) {
                    camera.position.x += ySpeed*lookAtVector.y;
                    camera.position.y += ySpeed*lookAtVector.x;
                } else if (keyCode == 82) {
                    camera.position.set(0, -2, 0);
                    camera.lookAt(0,1000,0);
                }
            };
    
    
            //var angle=1;
            //angle++; // 角度をインクリメント
            //mesh.rotation.y = angle * Math.PI / 180; // 回転
            // マウスのX座標がステージの幅の何%の位置にあるか調べてそれを360度で乗算する
            //var targetRot = (mouseX / window.innerWidth) * 360;
            // 値 += (目標値 - 現在の値) * 減速値
            //rot += (targetRot - rot) * 0.02;
            // ラジアンに変換する
            //var radian = rot * Math.PI / 180;
            // 角度に応じてカメラの位置を設定
            //camera.position.x = 1000 * Math.sin(radian);
            //camera.position.z = 1000 * Math.cos(radian);
            // 原点方向を見つめる
            //camera.lookAt(new THREE.Vector3(0, 0, 0));
            
            //x_coord = x_coord + 0.1;
            // box.rotation.y += 0.0;
            // spheremesh.rotation.x += 0.0;
    
    
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

    
    
//            joystick_left.addEventListener('touchStartValidation', function(event){
//                var touch	= event.changedTouches[0];
//                event.preventDefault();
//                if( Math.abs(touch.pageX-joystick_left.baseX) < 100 &&   Math.abs(touch.pageY-joystick_left.baseY) < 100 ){
//
//                    vz = joystick_left.deltaY();
//                    vrot = joystick_left.deltaX();
//                    var dif1 = touch.pageX-joystick_left.baseX;
//                    var dif2 = touch.pageY-joystick_left.baseY;
//
//                    console.log("left")
//                    console.log(dif1,fid2)
//                    return true;
//                }else{
//                    vx = 0.0;
//                    vy = 0.0;
//
//                    var dif1 = touch.pageX-joystick_left.baseX;
//                    var dif2 = touch.pageY-joystick_left.baseY;
//
//                    console.log("left_zero")
//                    console.log(dif1,fid2)
//                    return true;
//                };
//
//                //if( touch.pageY < window.innerHeight   )	return false;
//                //if( touch.pageX < window.innerWidth-250   )	{
//                //    
//                //    return false;
//                //}
//                    
//                //return true
//            });
//    
//            joystick_right.addEventListener('touchStartValidation', function(event){
//                var touch	= event.changedTouches[0];
//                event.preventDefault();
//                if( Math.abs(touch.pageX-joystick_right.baseX) < 100 &&   Math.abs(touch.pageY-joystick_right.baseY) < 100 ){
//                    
//                    vx = joystick_right.deltaX();
//                    vy = joystick_right.deltaY();
//    
//                    var dif1 = touch.pageX-joystick_left.baseX;
//                    var dif2 = touch.pageY-joystick_left.baseY;
//
//                    console.log("left")
//                    console.log(dif1,fid2)
//                    return true;
//                }else{
//                    vx = 0.0;
//                    vy = 0.0;
//    
//                    var dif1 = touch.pageX-joystick_right.baseX;
//                    var dif2 = touch.pageY-joystick_right.baseY;
//
//                    console.log("right_zero")
//                    console.log(dif1,fid2)
//                    return false;
//                };
////                if( touch.pageY < window.innerHeight   )	return false;
////                if( touch.pageX < window.innerWidth-250 )	{
////                    vz = 0.0;
////                    vrot = 0.0;
////                    return false;
////                }
////                return true
//            });
//    
    
    
            if(Math.abs(vz) > Math.abs(vrot)) {
                vrot = 0.0;
            }else{
                vz = 0.0;
            }        
            
            vrot = Math.PI * vrot/180;
            rot +=vrot;
    //        if(Math.abs(vx) > Math.abs(vy)) {
    //            vx = vx*(Math.cos(rot-Math.PI/2.0)*nx-Math.sin(rot-Math.PI/2.0)*ny);
    //            vy = vx*(Math.sin(rot-Math.PI/2.0)*nx+Math.cos(rot-Math.PI/2.0)*ny);
    //        }else{
    //            vx = vy*(Math.cos(rot)*nx-Math.sin(rot)*ny);
    //            vy = vy*(Math.sin(rot)*nx+Math.cos(rot)*ny);
    //        };)
            
            if(Math.abs(vx) > Math.abs(vy)) {
                vy = 0;
                camera.position.x -= 0.0001*vx*Math.abs(vx)*lookAtVector.y;
                camera.position.y -= 0.0001*vx*Math.abs(vx)*lookAtVector.x;
            }else{
                vx = 0;
                camera.position.x -= 0.0001*vy*Math.abs(vy)*lookAtVector.x;
                camera.position.y -= 0.0001*vy*Math.abs(vy)*lookAtVector.y;
            };
    
    
    
            camera.position.z += -0.0001*vz*Math.abs(vz);
            camera.rotateY(-0.02*vrot*Math.abs(vrot));
    
    //        lookat_x = camera.position.x +0     ;
    //        lookat_y = camera.position.y +10000 ;
    //        lookat_z = 0.1;
    //        ax = lookat_x - camera.position.x;
    //        ay = lookat_y - camera.position.y;
    //
    //        lookat_x = camera.position.x + Math.cos(rot)*ax - Math.sin(rot)*ay ;
    //        lookat_y = camera.position.y + Math.sin(rot)*ax + Math.cos(rot)*ay ;
    //        lookat_z = 0.0;
    
    //        camera.lookAt(new THREE.Vector3(lookat_x,lookat_y,lookat_z));
            
            //camera.lookAt(new THREE.Vector3(lookat_x, lookat_y, lookat_z));
    
    
            if (camera.position.z < 0.3) { 
                camera.position.z = 0.3;
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
    
            
            //camera.position.x = 1 * Math.sin(1 * Math.PI / 180);
            //camera.position.z = 1 * Math.cos(1 * Math.PI / 180);
            // camera.lookAt(new THREE.Vector3(0, 0, 0));
            
                    
            renderer.render(scene, camera); // レンダリング
    
            // joystick_right.deltaX=100 - joystick_right.baseX;
            // joystick_right.deltaY=100 - joystick_right.baseY;
            
    
            //joystick_right.deltaX()=0.0;
            //joystick_right.deltaY()=0.0;
            //joystick_right.baseX()=0.0;
            //joystick_right.baseY()=0.0;
            light.position.x = controls.light_x;
            light.position.y = controls.light_y;
            light.position.z = controls.light_z;
    
            light.power = controls.strength;
            //inter_row = controls.inter_row;
            //intra_row = controls.intra_row;
            soilmesh.position.z = controls.ground_level;
            grassmesh.position.z = soilmesh.position.z-0.2;
    
             
          }
        }