import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
    initializeWindow
    initializeOpenGL
    initializeCallbacks

    mainLoop

initializeWindow = do
    _ <- getArgsAndInitialize
    initialWindowSize $= Size 800 600
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hasteroids"

-- | Parametros iniciais do OpenGL
initializeOpenGL = do
    -- Desabilita check de profundidade -> Jogo 2D
    depthMask $= Disabled

    -- Linhas mais suaves
    lineSmooth  $= Enabled
    blend       $= Enabled
    blendFunc   $= (SrcAlpha,OneMinusSrcAlpha)
    lineWidth   $= 2.0

    -- Configura o Viewport
    viewport   $= (Position 0 0, Size 800 600)

    -- Configura uma projeção ortogonal para renderização 2D
    matrixMode $= Projection
    loadIdentity
    ortho 0 800 600 0 (-1) 1
    matrixMode $= Modelview 0
    loadIdentity

    -- Cor do background - float(R) float(G) float(B) float(A)
    clearColor $= Color4 0.0 0.0 0.1 1.0

-- GLUT - renderizador
initializeCallbacks = do
    displayCallback $= render


render = do
    clear [ColorBuffer]
    swapBuffers
