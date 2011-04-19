Assimp
======

FFI bindings for [Assimp](http://assimp.sourceforge.net) - asset import library. Supports the following formats:

- Collada ( *.dae;*.xml )
- Blender ( *.blend ) 3
- Biovision BVH ( *.bvh ) 
- 3D Studio Max 3DS ( *.3ds ) 
- 3D Studio Max ASE ( *.ase ) 
- Wavefront Object ( *.obj ) 
- Stanford Polygon Library ( *.ply ) 
- AutoCAD DXF ( *.dxf ) 2
- Neutral File Format ( *.nff ) 
- Sense8 WorldToolkit ( *.nff ) 
- Valve Model ( *.smd,*.vta ) 3 
- Quake I ( *.mdl ) 
- Quake II ( *.md2 ) 
- Quake III ( *.md3 ) 
- Quake 3 BSP ( *.pk3 ) 1 
- RtCW ( *.mdc )
- Doom 3 ( *.md5mesh;*.md5anim;*.md5camera ) 
- DirectX X ( *.x )
- Quick3D ( *.q3o;*q3s )
- Raw Triangles ( *.raw )
- AC3D ( *.ac )
- Stereolithography ( *.stl )
- Autodesk DXF ( *.dxf )
- Irrlicht Mesh ( *.irrmesh;*.xml )
- Irrlicht Scene ( *.irr;*.xml )
- Object File Format ( *.off )
- Terragen Terrain ( *.ter ) 
- 3D GameStudio Model ( *.mdl ) 
- 3D GameStudio Terrain ( *.hmp )
- Ogre (*.mesh.xml, *.skeleton.xml, *.material)3 
- Milkshape 3D ( *.ms3d )
- LightWave Model ( *.lwo )
- LightWave Scene ( *.lws )
- Modo Model ( *.lxo )
- CharacterStudio Motion ( *.csm )
- Stanford Ply ( *.ply )
- TrueSpace ( *.cob, *.scn ) 2

1: Experimental loaders

2: Indicates very limited support - many of the format's features don't map to Assimp's data structures.

3: These formats support animations, but ASSIMP doesn't yet support them (or they're buggy)

Example
-------

All data structures mirror those in the original library. See [the
documentation](http://assimp.sourceforge.net/lib_html/index.html) for
more information.

    -- Post-processing flags.
    processing = CalcTangentSpace
             .|. Triangulate
             .|. JoinIdenticalVertices
             .|. SortByPType

    -- Just call `importFile` with the name of the file and the
    -- post-processing flags.
    main = do
      args <- getArgs
      scene <- importFile "model.ply" processing
