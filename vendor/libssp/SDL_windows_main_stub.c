#include <SDL.h>
#include <windows.h>

int SDL_main(int argc, char *argv[]);

// Provide a minimal WinMain entry point that forwards to SDL_main without
// enabling stack protector instrumentation.
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    (void)hInstance;
    (void)hPrevInstance;
    (void)lpCmdLine;
    (void)nCmdShow;
    SDL_SetMainReady();
    return SDL_main(__argc, __argv);
}
