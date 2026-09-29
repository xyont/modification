/* render.c -- sokol_gfx pipelines, buffers and colormaps (GL 4.1 core). */
#include "render.h"
#include "sokol_gfx.h"
#include <math.h>

void cv_gl_enable_point_size(void);   /* sokol_impl.c: glEnable(GL_PROGRAM_POINT_SIZE) */

/* ---- colormaps ---------------------------------------------------------------
   Fast and CoolWarm: Moreland's published tables (kennethmoreland.com/color-advice).
   Viridis and Turbo: the usual polynomial fits. Rainbow last, on purpose.

   Inferno, Jet, Grayscale, Rainbow-desaturated and Classic are ported from
   CalculiX GraphiX (cgx) to keep the two viewers consistent.

   Table sizes were chosen per colormap:
     - Inferno: 32 entries (sampled every 8th of the 256-entry BIDS table).
       The curve is smooth and monotonic, so linear interpolation between
       these 32 points is visually identical to a full 256-entry lookup.
     - Rainbow-desaturated: 64 entries (sampled every 4th of the 256-entry
       ParaView table). It has sharper transitions around t~0.3 and t~0.7,
       where 32 entries would band.
     - Jet, Grayscale and Classic: analytic, no table needed.

   The original cgx code stores its tables on the stack; here they are static
   const so nothing is copied at each call. */

const char* const cv_cmap_names[CV_CMAP_N] = {
    "Fast", "Cool-warm", "Viridis", "Turbo", "Heat", "Rainbow",
    "Inferno", "Jet", "Grayscale", "Rainbow-desaturated", "Classic"
};

static const float kFast[32][3] = {
    {0.0549f,0.0549f,0.4706f},{0.1098f,0.1373f,0.5333f},{0.1490f,0.2118f,0.5961f},{0.1843f,0.2863f,0.6588f},
    {0.2118f,0.3608f,0.7216f},{0.2353f,0.4353f,0.7882f},{0.2667f,0.5098f,0.8353f},{0.2980f,0.5804f,0.8706f},
    {0.3255f,0.6510f,0.9098f},{0.3529f,0.7255f,0.9451f},{0.4314f,0.7765f,0.9490f},{0.5176f,0.8235f,0.9412f},
    {0.5922f,0.8667f,0.9294f},{0.6627f,0.9137f,0.9216f},{0.7569f,0.9333f,0.8745f},{0.8549f,0.9412f,0.8039f},
    {0.9137f,0.9255f,0.7216f},{0.9373f,0.8824f,0.6275f},{0.9529f,0.8431f,0.5333f},{0.9529f,0.7882f,0.4667f},
    {0.9451f,0.7333f,0.4157f},{0.9373f,0.6745f,0.3608f},{0.9255f,0.6157f,0.3098f},{0.8980f,0.5529f,0.2745f},
    {0.8667f,0.4941f,0.2392f},{0.8392f,0.4314f,0.2039f},{0.8078f,0.3686f,0.1686f},{0.7686f,0.3137f,0.1529f},
    {0.7216f,0.2627f,0.1451f},{0.6784f,0.2078f,0.1373f},{0.6353f,0.1490f,0.1294f},{0.5882f,0.0784f,0.1176f},
};
static const float kCoolWarm[32][3] = {
    {0.2314f,0.2980f,0.7529f},{0.2667f,0.3569f,0.8039f},{0.3059f,0.4118f,0.8471f},{0.3451f,0.4627f,0.8863f},
    {0.3882f,0.5176f,0.9216f},{0.4314f,0.5647f,0.9490f},{0.4745f,0.6118f,0.9725f},{0.5216f,0.6588f,0.9882f},
    {0.5647f,0.6980f,0.9961f},{0.6078f,0.7373f,1.0000f},{0.6549f,0.7686f,0.9961f},{0.6980f,0.8000f,0.9843f},
    {0.7373f,0.8235f,0.9686f},{0.7765f,0.8431f,0.9451f},{0.8157f,0.8549f,0.9176f},{0.8510f,0.8627f,0.8863f},
    {0.8824f,0.8588f,0.8431f},{0.9137f,0.8392f,0.7961f},{0.9373f,0.8118f,0.7451f},{0.9529f,0.7843f,0.6980f},
    {0.9647f,0.7490f,0.6471f},{0.9686f,0.7059f,0.5961f},{0.9686f,0.6627f,0.5451f},{0.9608f,0.6157f,0.4941f},
    {0.9490f,0.5647f,0.4431f},{0.9294f,0.5098f,0.3961f},{0.9020f,0.4510f,0.3490f},{0.8745f,0.3882f,0.3059f},
    {0.8392f,0.3216f,0.2627f},{0.8000f,0.2471f,0.2235f},{0.7529f,0.1608f,0.1843f},{0.7059f,0.0157f,0.1490f},
};

/* --- ported from cgx: Inferno (BIDS), 32 entries (sampled every 8th) ------- */
static const float kInferno[32][3] = {
    {0.00146159096f, 0.00046612777f, 0.01386552000f},  /*   0 */
    {0.01399503880f, 0.01122471380f, 0.07186168900f},  /*   8 */
    {0.04225255540f, 0.02813850150f, 0.14114051900f},  /*  16 */
    {0.08196207730f, 0.04332786660f, 0.21528911300f},  /*  24 */
    {0.12928498400f, 0.04729308380f, 0.29078801200f},  /*  32 */
    {0.17649320200f, 0.04140170890f, 0.34811060600f},  /*  40 */
    {0.22476290800f, 0.03407910530f, 0.40442240800f},  /*  48 */
    {0.27134666400f, 0.02473208710f, 0.45655329300f},  /*  56 */
    {0.31328183500f, 0.01439333980f, 0.50456812400f},  /*  64 */
    {0.34859177100f, 0.00430726461f, 0.54754880500f},  /*  72 */
    {0.37667451100f, 0.00000000000f, 0.58515321300f},  /*  80 */
    {0.39782490100f, 0.00490665370f, 0.61744165900f},  /*  88 */
    {0.41287229500f, 0.02102384700f, 0.64457442300f},  /*  96 */
    {0.42289521900f, 0.04580117170f, 0.66682309600f},  /* 104 */
    {0.42909014100f, 0.07625397910f, 0.68448189900f},  /* 112 */
    {0.43248009700f, 0.10995076700f, 0.69779172200f},  /* 120 */
    {0.43410288100f, 0.14516091200f, 0.70700526600f},  /* 128 */
    {0.43454616800f, 0.18071183600f, 0.71241825900f},  /* 136 */
    {0.43469175400f, 0.21611680700f, 0.71455450100f},  /* 144 */
    {0.43502179700f, 0.25167491600f, 0.71397157200f},  /* 152 */
    {0.43760157200f, 0.28820235000f, 0.71122561700f},  /* 160 */
    {0.44501661900f, 0.32726450200f, 0.70723466200f},  /* 168 */
    {0.46008347400f, 0.37121791200f, 0.70340515000f},  /* 176 */
    {0.48550762200f, 0.42191985000f, 0.70133824100f},  /* 184 */
    {0.52222935800f, 0.48088923700f, 0.70239646300f},  /* 192 */
    {0.57301354500f, 0.54824150700f, 0.70778664600f},  /* 200 */
    {0.63934074300f, 0.62361464600f, 0.71839693600f},  /* 208 */
    {0.72209240200f, 0.70598892800f, 0.73498717000f},  /* 216 */
    {0.82228462800f, 0.79514416000f, 0.75846242300f},  /* 224 */
    {0.93734175500f, 0.89006038800f, 0.78917441000f},  /* 232 */
    {1.00000000000f, 0.97608168200f, 0.82867568100f},  /* 240 */
    {0.98836206800f, 0.99836414300f, 0.64492400500f},  /* 248 */
};

/* --- ported from cgx: Rainbow-desaturated (ParaView), 64 entries ------------ */
static const float kRainbowDesaturated[64][3] = {
    {0.278431f, 0.278431f, 0.858824f},  /*   0 */
    {0.297377f, 0.318463f, 0.905882f},  /*   4 */
    {0.316323f, 0.358495f, 0.952941f},  /*   8 */
    {0.335269f, 0.398527f, 1.000000f},  /*  12 */
    {0.354215f, 0.438559f, 0.964706f},  /*  16 */
    {0.373161f, 0.478591f, 0.917647f},  /*  20 */
    {0.392107f, 0.518623f, 0.870588f},  /*  24 */
    {0.411053f, 0.558655f, 0.823529f},  /*  28 */
    {0.429999f, 0.598687f, 0.776471f},  /*  32 */
    {0.448945f, 0.638719f, 0.729412f},  /*  36 */
    {0.467891f, 0.678751f, 0.682353f},  /*  40 */
    {0.486837f, 0.718783f, 0.635294f},  /*  44 */
    {0.505783f, 0.758815f, 0.588235f},  /*  48 */
    {0.524729f, 0.798847f, 0.541176f},  /*  52 */
    {0.543675f, 0.838879f, 0.494118f},  /*  56 */
    {0.562621f, 0.878911f, 0.447059f},  /*  60 */
    {0.581567f, 0.918943f, 0.400000f},  /*  64 */
    {0.600513f, 0.958975f, 0.352941f},  /*  68 */
    {0.619459f, 0.999007f, 0.305882f},  /*  72 */
    {0.638405f, 0.978991f, 0.258824f},  /*  76 */
    {0.657351f, 0.938959f, 0.211765f},  /*  80 */
    {0.676297f, 0.898927f, 0.164706f},  /*  84 */
    {0.695243f, 0.858895f, 0.117647f},  /*  88 */
    {0.714189f, 0.818863f, 0.070588f},  /*  92 */
    {0.733135f, 0.778831f, 0.023529f},  /*  96 */
    {0.752081f, 0.738799f, 0.000000f},  /* 100 */
    {0.771027f, 0.698767f, 0.000000f},  /* 104 */
    {0.789973f, 0.658735f, 0.000000f},  /* 108 */
    {0.808919f, 0.618703f, 0.000000f},  /* 112 */
    {0.827865f, 0.578671f, 0.000000f},  /* 116 */
    {0.846811f, 0.538639f, 0.000000f},  /* 120 */
    {0.865757f, 0.498607f, 0.000000f},  /* 124 */
    {0.884703f, 0.458575f, 0.000000f},  /* 128 */
    {0.903649f, 0.418543f, 0.000000f},  /* 132 */
    {0.922595f, 0.378511f, 0.000000f},  /* 136 */
    {0.941541f, 0.338479f, 0.000000f},  /* 140 */
    {0.960487f, 0.298447f, 0.000000f},  /* 144 */
    {0.979433f, 0.258415f, 0.000000f},  /* 148 */
    {0.998379f, 0.218383f, 0.000000f},  /* 152 */
    {1.000000f, 0.178351f, 0.000000f},  /* 156 */
    {1.000000f, 0.138319f, 0.000000f},  /* 160 */
    {1.000000f, 0.098287f, 0.000000f},  /* 164 */
    {1.000000f, 0.058255f, 0.000000f},  /* 168 */
    {1.000000f, 0.018223f, 0.000000f},  /* 172 */
    {0.979433f, 0.000000f, 0.000000f},  /* 176 */
    {0.941541f, 0.000000f, 0.000000f},  /* 180 */
    {0.903649f, 0.000000f, 0.000000f},  /* 184 */
    {0.865757f, 0.000000f, 0.000000f},  /* 188 */
    {0.827865f, 0.000000f, 0.000000f},  /* 192 */
    {0.789973f, 0.000000f, 0.000000f},  /* 196 */
    {0.752081f, 0.000000f, 0.000000f},  /* 200 */
    {0.714189f, 0.000000f, 0.000000f},  /* 204 */
    {0.676297f, 0.000000f, 0.000000f},  /* 208 */
    {0.638405f, 0.000000f, 0.000000f},  /* 212 */
    {0.600513f, 0.000000f, 0.000000f},  /* 216 */
    {0.562621f, 0.000000f, 0.000000f},  /* 220 */
    {0.524729f, 0.000000f, 0.000000f},  /* 224 */
    {0.486837f, 0.000000f, 0.000000f},  /* 228 */
    {0.448945f, 0.000000f, 0.000000f},  /* 232 */
    {0.411053f, 0.000000f, 0.000000f},  /* 236 */
    {0.373161f, 0.000000f, 0.000000f},  /* 240 */
    {0.335269f, 0.000000f, 0.000000f},  /* 244 */
    {0.297377f, 0.000000f, 0.000000f},  /* 248 */
    {0.278431f, 0.000000f, 0.000000f},  /* 252 */
};

static float clamp01(float x) { return x < 0 ? 0 : x > 1 ? 1 : x; }

/* --- cmap helpers ------------------------------------------------------------- */

static void cmap_inferno(float t, float o[3]) {
    float x = t * 31.f;                      /* 32 entries -> 31 segments */
    int i = (int)x; if (i > 30) i = 30;
    float f = x - (float)i;
    for (int k = 0; k < 3; k++)
        o[k] = kInferno[i][k] + (kInferno[i + 1][k] - kInferno[i][k]) * f;
}

/* Jet: analytic piecewise, ported from cgx (Rafal Brzegowy). */
static void cmap_jet(float v, float o[3]) {
    /* R */
    if      (v <= 0.375f) o[0] = 0.f;
    else if (v <= 0.625f) o[0] = (v - 0.375f) / 0.25f;
    else if (v <= 0.875f) o[0] = 1.f;
    else                  o[0] = 1.f - ((v - 0.875f) / 0.125f) * 0.5f;
    /* G */
    if      (v <= 0.125f) o[1] = 0.f;
    else if (v <= 0.375f) o[1] = (v - 0.125f) / 0.25f;
    else if (v <= 0.625f) o[1] = 1.f;
    else if (v <= 0.875f) o[1] = (0.875f - v) / 0.25f;
    else                  o[1] = 0.f;
    /* B */
    if      (v <= 0.125f) o[2] = 0.5f + (v / 0.125f) * 0.5f;
    else if (v <= 0.375f) o[2] = 1.f;
    else if (v <= 0.625f) o[2] = (0.625f - v) / 0.25f;
    else                  o[2] = 0.f;
}

/* Grayscale: computed arithmetically (no table needed). */
static void cmap_grayscale(float t, float o[3]) {
    int i = (int)(t * 255.f + 0.5f);
    if (i < 0) i = 0; if (i > 255) i = 255;
    float g = (float)i / 255.f;
    o[0] = o[1] = o[2] = g;
}

static void cmap_rainbow_desaturated(float t, float o[3]) {
    float x = t * 63.f;                      /* 64 entries -> 63 segments */
    int i = (int)x; if (i > 62) i = 62;
    float f = x - (float)i;
    for (int k = 0; k < 3; k++)
        o[k] = kRainbowDesaturated[i][k]
             + (kRainbowDesaturated[i + 1][k] - kRainbowDesaturated[i][k]) * f;
}

/* Classic GraphiX colormap, ported from cgx. */
static void cmap_classic(float v, float o[3]) {
    /* R */
    if      (v < 0.1f) o[0] = 0.5f - v / 0.2f;
    else if (v < 0.5f) o[0] = 0.f;
    else if (v < 0.8f) o[0] = (v - 0.5f) / 0.3f;
    else if (v < 0.9f) o[0] = 1.f;
    else               o[0] = 1.f - (v - 0.9f) / 0.4f;
    /* G */
    if      (v < 0.1f) o[1] = 0.f;
    else if (v < 0.3f) o[1] = (v - 0.1f) / 0.2f;
    else if (v < 0.5f) o[1] = 1.f - (v - 0.3f) / 0.4f;
    else if (v < 0.8f) o[1] = 0.5f + (v - 0.5f) / 0.6f;
    else               o[1] = 1.f - (v - 0.8f) / 0.2f;
    /* B */
    if (v < 0.5f) o[2] = 1.f - v / 0.5f;
    else          o[2] = 0.f;
}

void cv_colormap_rgb(int cm, float t, float o[3]) {
    t = clamp01(t);
    switch (cm) {
        case CV_CMAP_FAST: case CV_CMAP_COOLWARM: {
            const float (*tb)[3] = cm == CV_CMAP_FAST ? kFast : kCoolWarm;
            float x = t * 31.f;
            int i = (int)x; if (i > 30) i = 30;
            float f = x - (float)i;
            for (int k = 0; k < 3; k++) o[k] = tb[i][k] + (tb[i + 1][k] - tb[i][k]) * f;
            return;
        }
        case CV_CMAP_VIRIDIS: {
            static const float c[7][3] = {
                {0.2777273f, 0.0054073f, 0.3340998f}, {0.1050930f, 1.4046135f, 1.3845902f},
                {-0.3308618f, 0.2148476f, 0.0950952f}, {-4.6342305f, -5.7991010f, -19.3324410f},
                {6.2282699f, 14.1799334f, 56.6905526f}, {4.7763850f, -13.7451454f, -65.3530326f},
                {-5.4354559f, 4.6458526f, 26.3124352f},
            };
            for (int k = 0; k < 3; k++) {
                float v = c[6][k];
                for (int j = 5; j >= 0; j--) v = c[j][k] + t * v;
                o[k] = clamp01(v);
            }
            return;
        }
        case CV_CMAP_TURBO: {
            float x = t, x2 = x * x, x3 = x2 * x, x4 = x2 * x2, x5 = x4 * x;
            o[0] = clamp01(0.13572138f + 4.61539260f * x - 42.66032258f * x2 + 132.13108234f * x3 - 152.94239396f * x4 + 59.28637943f * x5);
            o[1] = clamp01(0.09140261f + 2.19418839f * x + 4.84296658f * x2 - 14.18503333f * x3 + 4.27729857f * x4 + 2.82956604f * x5);
            o[2] = clamp01(0.10667330f + 12.64194608f * x - 60.58204836f * x2 + 110.36276771f * x3 - 89.90310912f * x4 + 27.34824973f * x5);
            return;
        }
        case CV_CMAP_HEAT: {                    /* black - red - yellow - white, for temperatures */
            static const float r[5][3] = { {0.05f,0,0.1f}, {0.6f,0.05f,0.05f}, {0.95f,0.4f,0}, {1,0.85f,0.2f}, {1,1,0.95f} };
            float s = t * 4.f;
            int i = (int)s; if (i > 3) i = 3;
            float f = s - (float)i;
            for (int k = 0; k < 3; k++) o[k] = r[i][k] + (r[i + 1][k] - r[i][k]) * f;
            return;
        }
        case CV_CMAP_RAINBOW: {
            static const float r[5][3] = { {0,0,1}, {0,1,1}, {0,1,0}, {1,1,0}, {1,0,0} };
            float s = t * 4.f;
            int i = (int)s; if (i > 3) i = 3;
            float f = s - (float)i;
            for (int k = 0; k < 3; k++) o[k] = r[i][k] + (r[i + 1][k] - r[i][k]) * f;
            return;
        }
        /* --- ported from cgx --- */
        case CV_CMAP_INFERNO:             cmap_inferno(t, o);             return;
        case CV_CMAP_JET:                 cmap_jet(t, o);                 return;
        case CV_CMAP_GRAYSCALE:           cmap_grayscale(t, o);           return;
        case CV_CMAP_RAINBOW_DESATURATED: cmap_rainbow_desaturated(t, o); return;
        case CV_CMAP_CLASSIC:             cmap_classic(t, o);             return;
    }
}

/* ---- shader ------------------------------------------------------------------ */

/* Desktop GL and WebGL2 (GLSL ES 3.00) share the source; only the header differs.
   Varyings match by name, so they carry no layout(location). */
#ifdef __EMSCRIPTEN__
#define GLSL_HDR "#version 300 es\nprecision highp float; precision highp int; precision highp sampler2D;\n"
#else
#define GLSL_HDR "#version 410\n"
#endif

static const char* kVS =
    GLSL_HDR
    "uniform mat4 u_mvp;\n"
    "uniform mat4 u_mv;\n"
    "uniform vec4 u_p;\n"                    /* x: deform scale, y: point size, z: on top, w: pull */
    "uniform vec4 u_q;\n"                    /* x: P[1][1] * viewport height (px -> view units), y: scale of a_disp2 */
    "in vec3 a_pos;\n"
    "in vec3 a_disp;\n"
    "in float a_scal;\n"
    "in vec3 a_disp2;\n"
    "out vec3 v_vpos;\n"
    "out float v_s;\n"
    "out float v_r;\n"                       /* point: sphere radius in view units */
    "out vec3 v_wpos;\n"                     /* deformed model-space position, for the clip plane */
    "void main() {\n"
    "  vec3 p = a_pos + a_disp * u_p.x + a_disp2 * u_q.y;\n"
    "  gl_Position = u_mvp * vec4(p, 1.0);\n"
    /* "on top": squeeze depth into the front 2% of the range, so these points
       pass in front of the faces yet still hide one another near-to-far */
    "  if (u_p.z > 0.5) gl_Position.z = -gl_Position.w + (gl_Position.z + gl_Position.w) * 0.02;\n"
    /* edges lie ON faces: pulled a hair toward the eye so they win against them */
    "  gl_Position.z -= u_p.w * gl_Position.w;\n"
    "  v_r = u_p.y * gl_Position.w / max(u_q.x, 1e-6);\n"
    "  v_vpos = (u_mv * vec4(p, 1.0)).xyz;\n"
    "  v_s = a_scal;\n"
    "  v_wpos = p;\n"
    "  gl_PointSize = u_p.y;\n"
    "}\n";

static const char* kFS =
    GLSL_HDR
    "uniform vec4 u_color;\n"                /* solid colour */
    "uniform vec4 u_rng;\n"                  /* min, max, bands, mode */
    "uniform vec4 u_flags;\n"                /* x: grey out of range, y: shade, z: on top */
    "uniform vec4 u_pz;\n"                   /* projection: z_clip = x*z + y, w_clip = z*z + w */
    "uniform vec4 u_clip;\n"                 /* clip plane normal, d; w = 1e30 when off */
    "uniform sampler2D u_cmap;\n"
    "uniform sampler2D u_etex;\n"
    "in vec3 v_vpos;\n"
    "in float v_s;\n"
    "in float v_r;\n"
    "in vec3 v_wpos;\n"
    "out vec4 frag;\n"
    "void main() {\n"
    "  if (dot(v_wpos, u_clip.xyz) > u_clip.w) discard;\n"
    /* Points are balls: each disc pixel takes the depth of the ball's surface there,
       so a point behind a face is hidden, one crossing it shows only its front part,
       and one in front shows whole. */
    "#ifdef SPHERE\n"
    "  vec2 pc = gl_PointCoord * 2.0 - 1.0;\n"
    "  float d2 = dot(pc, pc);\n"
    "  if (d2 > 1.0) discard;\n"
    "  float nz = sqrt(1.0 - d2);\n"
    "  float vz = v_vpos.z + nz * v_r;\n"
    "  float zd = (u_pz.x * vz + u_pz.y) / (u_pz.z * vz + u_pz.w) * 0.5 + 0.5;\n"
    "  gl_FragDepth = u_flags.z > 0.5 ? zd * 0.02 : zd;\n"
    "#endif\n"
    "  int mode = int(u_rng.w + 0.5);\n"
    "  vec3 c = u_color.rgb;\n"
    "  if (false) {\n"
    "  } else if (mode > 0) {\n"
    "    float s = v_s;\n"
    "#ifdef PRIM\n"
    "    if (mode == 2) {\n"
    "      int id = gl_PrimitiveID;\n"
    "      s = texelFetch(u_etex, ivec2(id & 4095, id >> 12), 0).r;\n"
    "    }\n"
    "#endif\n"
    "    if (isnan(s)) {\n"
    "      c = vec3(0.62, 0.62, 0.60);\n"      /* no data */
    "    } else {\n"
    "      float t = (s - u_rng.x) / max(u_rng.y - u_rng.x, 1e-30);\n"
    "      bool out_ = t < -1e-4 || t > 1.0001;\n"
    "      t = clamp(t, 0.0, 1.0);\n"
    /* keep in step with cv_band_center() in field.h */
    "      if (u_rng.z > 0.5) { float b = u_rng.z; t = (min(floor(t * b), b - 1.0) + 0.5) / b; }\n"
    "      c = textureLod(u_cmap, vec2(t, 0.5), 0.0).rgb;\n"
    "      if (out_ && u_flags.x > 0.5) c = vec3(0.45);\n"
    "    }\n"
    "  }\n"
    "  if (u_flags.y > 0.5) {\n"
    "    vec3 n = normalize(cross(dFdx(v_vpos), dFdy(v_vpos)));\n"
    "    c *= 0.30 + 0.70 * abs(n.z);\n"
    "  }\n"
    "#ifdef SPHERE\n"
    "  c *= 0.72 + 0.28 * nz;\n"               /* a touch of rim darkening: reads as a ball */
    "#endif\n"
    "  frag = vec4(c, 1.0);\n"
    "}\n";

typedef struct { float mvp[16]; float mv[16]; float p[4]; float q[4]; } vs_params;
typedef struct { float color[4]; float rng[4]; float flags[4]; float pz[4]; float clip[4]; } fs_params;

/* ---- state ------------------------------------------------------------------- */

enum { ETEX_W = 4096 };

static struct {
    sg_shader   shd, shd_prim, shd_pt;
    sg_pipeline pip_tri, pip_tri_prim, pip_line, pip_pt;
    sg_pipeline pip_tri_ni, pip_line_ni, pip_pt_ni;   /* non-indexed: the aux vertex sets */
    sg_image    cmap_img;  sg_view cmap_view;
    sg_image    etex_img;  sg_view etex_view;
    sg_buffer   ib_grp;               /* skin triangles ordered by group */
    uint32_t*   grp_first;            /* ngroups + 1 */
    float*      grp_rgb;              /* 3 per group */
    int         ngroups;
    sg_sampler  smp_lin, smp_near;
    sg_buffer   pos, disp, disp2, scal;
    sg_buffer   ib_tri, ib_edge, ib_pt;
    size_t      n_tri, n_edge, n_pt;
    uint32_t    n_nodes;
} R;

static void kill_buf(sg_buffer* b) {
    if (b->id) sg_destroy_buffer(*b);
    b->id = 0;
}

static sg_buffer make_buf(const void* p, size_t bytes, bool index) {
    if (!p || bytes == 0) return (sg_buffer){0};
    sg_buffer_desc d = { .data = { p, bytes } };
    if (index) { d.usage.index_buffer = true; d.usage.vertex_buffer = false; }
    else d.usage.vertex_buffer = true;
    return sg_make_buffer(&d);
}

void cv_render_groups(const uint32_t* tri, size_t n_tri, const uint32_t* first, const float* rgb, int ng) {
    kill_buf(&R.ib_grp);
    free(R.grp_first); free(R.grp_rgb);
    R.grp_first = NULL; R.grp_rgb = NULL; R.ngroups = 0;
    if (!tri || !n_tri || ng <= 0) return;
    R.grp_first = malloc(((size_t)ng + 1) * sizeof(uint32_t));
    R.grp_rgb = malloc((size_t)ng * 3 * sizeof(float));
    if (!R.grp_first || !R.grp_rgb) { free(R.grp_first); free(R.grp_rgb); R.grp_first = NULL; R.grp_rgb = NULL; return; }
    memcpy(R.grp_first, first, ((size_t)ng + 1) * sizeof(uint32_t));
    memcpy(R.grp_rgb, rgb, (size_t)ng * 3 * sizeof(float));
    R.ib_grp = make_buf(tri, n_tri * 12, true);
    R.ngroups = R.ib_grp.id ? ng : 0;
}

static void make_etex(const float* v, int w, int h) {
    if (R.etex_view.id) sg_destroy_view(R.etex_view);
    if (R.etex_img.id) sg_destroy_image(R.etex_img);
    R.etex_img = sg_make_image(&(sg_image_desc){
        .width = w, .height = h, .pixel_format = SG_PIXELFORMAT_R32F,
        .data.mip_levels[0] = { v, (size_t)w * (size_t)h * sizeof(float) },
    });
    R.etex_view = sg_make_view(&(sg_view_desc){ .texture.image = R.etex_img });
}

void cv_render_colormap(int cm, bool reverse, bool grey) {
    uint8_t px[256 * 4];
    for (int i = 0; i < 256; i++) {
        float c[3];
        cv_colormap_rgb(cm, reverse ? 1.f - (float)i / 255.f : (float)i / 255.f, c);
        if (grey) c[0] = c[1] = c[2] = 0.2126f * c[0] + 0.7152f * c[1] + 0.0722f * c[2];
        for (int k = 0; k < 3; k++) px[4 * i + k] = (uint8_t)(c[k] * 255.f + 0.5f);
        px[4 * i + 3] = 255;
    }
    if (R.cmap_view.id) sg_destroy_view(R.cmap_view);
    if (R.cmap_img.id) sg_destroy_image(R.cmap_img);
    R.cmap_img = sg_make_image(&(sg_image_desc){
        .width = 256, .height = 1, .pixel_format = SG_PIXELFORMAT_RGBA8,
        .data.mip_levels[0] = { px, sizeof px },
    });
    R.cmap_view = sg_make_view(&(sg_view_desc){ .texture.image = R.cmap_img });
}

static sg_shader make_shader(const char* fs_src) {
    return sg_make_shader(&(sg_shader_desc){
        .vertex_func.source = kVS,
        .fragment_func.source = fs_src,
        .attrs = {
            [0] = { .glsl_name = "a_pos" },
            [1] = { .glsl_name = "a_disp" },
            [2] = { .glsl_name = "a_scal" },
            [3] = { .glsl_name = "a_disp2" },
        },
        .uniform_blocks[0] = {
            .stage = SG_SHADERSTAGE_VERTEX, .size = sizeof(vs_params),
            .glsl_uniforms = {
                [0] = { .type = SG_UNIFORMTYPE_MAT4, .glsl_name = "u_mvp" },
                [1] = { .type = SG_UNIFORMTYPE_MAT4, .glsl_name = "u_mv" },
                [2] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_p" },
                [3] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_q" },
            },
        },
        .uniform_blocks[1] = {
            .stage = SG_SHADERSTAGE_FRAGMENT, .size = sizeof(fs_params),
            .glsl_uniforms = {
                [0] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_color" },
                [1] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_rng" },
                [2] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_flags" },
                [3] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_pz" },
                [4] = { .type = SG_UNIFORMTYPE_FLOAT4, .glsl_name = "u_clip" },
            },
        },
        .views = {
            [0].texture = { .stage = SG_SHADERSTAGE_FRAGMENT, .image_type = SG_IMAGETYPE_2D,
                            .sample_type = SG_IMAGESAMPLETYPE_FLOAT },
            [1].texture = { .stage = SG_SHADERSTAGE_FRAGMENT, .image_type = SG_IMAGETYPE_2D,
                            .sample_type = SG_IMAGESAMPLETYPE_UNFILTERABLE_FLOAT },
        },
        .samplers = {
            [0] = { .stage = SG_SHADERSTAGE_FRAGMENT, .sampler_type = SG_SAMPLERTYPE_FILTERING },
            [1] = { .stage = SG_SHADERSTAGE_FRAGMENT, .sampler_type = SG_SAMPLERTYPE_NONFILTERING },
        },
        .texture_sampler_pairs = {
            [0] = { .stage = SG_SHADERSTAGE_FRAGMENT, .view_slot = 0, .sampler_slot = 0, .glsl_name = "u_cmap" },
            [1] = { .stage = SG_SHADERSTAGE_FRAGMENT, .view_slot = 1, .sampler_slot = 1, .glsl_name = "u_etex" },
        },
        .label = "ccxview",
    });
}

void cv_render_init(void) {
    memset(&R, 0, sizeof R);
    cv_gl_enable_point_size();
    /* Three variants of one source. Only the per-element and group-colour modes
       read gl_PrimitiveID: under Mesa's llvmpipe (VMs, remote desktops, Xvfb) a
       fragment shader that reads it gets its varyings scrambled, which turned
       every contour a single colour. Keeping it out of the default shader keeps
       the everyday view correct on every driver. */
    const char* body = kFS + strlen(GLSL_HDR);
    R.shd = make_shader(kFS);
#ifdef __EMSCRIPTEN__
    R.shd_prim = R.shd;                       /* WebGL2 has no gl_PrimitiveID; the expanded stream is used instead */
#else
    {
        static char prim[8192];
        snprintf(prim, sizeof prim, "%s#define PRIM 1\n%s", GLSL_HDR, body);
        R.shd_prim = make_shader(prim);
    }
#endif
    /* the point variant: same source with SPHERE defined after the header */
    {
        static char sph[8192];
        snprintf(sph, sizeof sph, "%s#define SPHERE\n%s", GLSL_HDR, body);
        R.shd_pt = make_shader(sph);
    }

    sg_vertex_layout_state layout = {
        .buffers = { [0].stride = 12, [1].stride = 12, [2].stride = 4, [3].stride = 12 },
        .attrs = {
            [0] = { .buffer_index = 0, .format = SG_VERTEXFORMAT_FLOAT3 },
            [1] = { .buffer_index = 1, .format = SG_VERTEXFORMAT_FLOAT3 },
            [2] = { .buffer_index = 2, .format = SG_VERTEXFORMAT_FLOAT },
            [3] = { .buffer_index = 3, .format = SG_VERTEXFORMAT_FLOAT3 },
        },
    };
    sg_pipeline_desc pd = {
        .shader = R.shd,
        .layout = layout,
        .index_type = SG_INDEXTYPE_UINT32,
        .depth = { .compare = SG_COMPAREFUNC_LESS_EQUAL, .write_enabled = true },
    };
    pd.primitive_type = SG_PRIMITIVETYPE_TRIANGLES;
    R.pip_tri = sg_make_pipeline(&pd);        /* faces keep their true depth */
    pd.shader = R.shd_prim;
    R.pip_tri_prim = sg_make_pipeline(&pd);   /* per-element / group colours */
    pd.shader = R.shd;
    pd.primitive_type = SG_PRIMITIVETYPE_LINES;
    R.pip_line = sg_make_pipeline(&pd);
    pd.primitive_type = SG_PRIMITIVETYPE_POINTS;
    pd.shader = R.shd_pt;                     /* points are drawn as balls */
    R.pip_pt = sg_make_pipeline(&pd);
    /* the aux sets are plain vertex runs: no index buffer to build or upload */
    pd.index_type = SG_INDEXTYPE_NONE;
    R.pip_pt_ni = sg_make_pipeline(&pd);
    pd.primitive_type = SG_PRIMITIVETYPE_LINES;  pd.shader = R.shd;
    R.pip_line_ni = sg_make_pipeline(&pd);
    pd.primitive_type = SG_PRIMITIVETYPE_TRIANGLES;
    R.pip_tri_ni = sg_make_pipeline(&pd);

    R.smp_lin = sg_make_sampler(&(sg_sampler_desc){
        .min_filter = SG_FILTER_LINEAR, .mag_filter = SG_FILTER_LINEAR,
        .wrap_u = SG_WRAP_CLAMP_TO_EDGE, .wrap_v = SG_WRAP_CLAMP_TO_EDGE });
    R.smp_near = sg_make_sampler(&(sg_sampler_desc){
        .min_filter = SG_FILTER_NEAREST, .mag_filter = SG_FILTER_NEAREST,
        .wrap_u = SG_WRAP_CLAMP_TO_EDGE, .wrap_v = SG_WRAP_CLAMP_TO_EDGE });
    cv_render_colormap(CV_CMAP_FAST, false, false);
    float zero = 0;
    make_etex(&zero, 1, 1);
}

static void clear_aux(void);

void cv_render_clear_model(void) {
    clear_aux();
    kill_buf(&R.pos); kill_buf(&R.disp); kill_buf(&R.disp2); kill_buf(&R.scal);
    kill_buf(&R.ib_tri); kill_buf(&R.ib_edge); kill_buf(&R.ib_pt);
    R.n_tri = R.n_edge = R.n_pt = 0;
    R.n_nodes = 0;
}

void cv_render_shutdown(void) {
    cv_render_clear_model();
}

void cv_render_positions(const float* xyz, uint32_t n) {
    kill_buf(&R.pos);
    R.pos = make_buf(xyz, (size_t)n * 12, false);
    R.n_nodes = n;
}

void cv_render_displacement(const float* d, uint32_t n) {
    kill_buf(&R.disp);
    R.disp = make_buf(d, (size_t)n * 12, false);
}

void cv_render_displacement2(const float* d, uint32_t n) {
    kill_buf(&R.disp2);
    R.disp2 = make_buf(d, (size_t)n * 12, false);
}

void cv_render_scalar(const float* s, uint32_t n) {
    kill_buf(&R.scal);
    R.scal = make_buf(s, (size_t)n * 4, false);
}

void cv_render_tri_values(const float* v, size_t n_tri) {
    if (!v || n_tri == 0) { float z = 0; make_etex(&z, 1, 1); return; }
    int w = ETEX_W, h = (int)((n_tri + ETEX_W - 1) / ETEX_W);
    CV_ASSERT(h <= 16384);                       /* GL's usual texture limit: 67M triangles */
    size_t cells = (size_t)w * (size_t)h;
    if (cells == n_tri) { make_etex(v, w, h); return; }
    float* pad = malloc(cells * sizeof(float));
    if (!pad) { float z = 0; make_etex(&z, 1, 1); return; }
    memcpy(pad, v, n_tri * sizeof(float));
    for (size_t i = n_tri; i < cells; i++) pad[i] = 0;
    make_etex(pad, w, h);
    free(pad);
}

void cv_render_indices(const uint32_t* tri, size_t n_tri, const uint32_t* edge, size_t n_edge,
                       const uint32_t* pt, size_t n_pt) {
    kill_buf(&R.ib_tri); kill_buf(&R.ib_edge); kill_buf(&R.ib_pt);
    R.ib_tri = make_buf(tri, n_tri * 12, true);   R.n_tri = R.ib_tri.id ? n_tri : 0;
    R.ib_edge = make_buf(edge, n_edge * 8, true); R.n_edge = R.ib_edge.id ? n_edge : 0;
    R.ib_pt = make_buf(pt, n_pt * 4, true);       R.n_pt = R.ib_pt.id ? n_pt : 0;
}

typedef struct { sg_buffer pos, disp, scal, disp2; } vset;

/* depth pull (NDC) for edges, which lie on faces; far too small to reach through a wall */
#define PULL 2e-5f

static void draw_layer(sg_pipeline pip, vset v, sg_buffer ib, int count, int mode, const float rgb[3],
                       bool shade, const cv_draw* d, float point_size, bool on_top, float pull, int first) {
    if (!v.pos.id || count <= 0) return;      /* ib is {0} for the non-indexed pipelines */
    /* no scalar buffer = nothing to colour with: fall back to solid */
    if (!v.scal.id && (mode == CV_COLOR_NODAL || mode == CV_COLOR_ELEM)) mode = CV_COLOR_SOLID;
    sg_apply_pipeline(pip);
    sg_bindings b = {
        .vertex_buffers = {
            [0] = v.pos,
            [1] = v.disp.id ? v.disp : v.pos,      /* no displacement: scale is 0 */
            [2] = v.scal.id ? v.scal : v.pos,
            [3] = v.disp2.id ? v.disp2 : v.pos,    /* no second part: its scale is 0 */
        },
        .index_buffer = ib,
        .views = { [0] = R.cmap_view, [1] = R.etex_view },
        .samplers = { [0] = R.smp_lin, [1] = R.smp_near },
    };
    sg_apply_bindings(&b);
    vs_params vs;
    memcpy(vs.mvp, d->mvp, sizeof vs.mvp);
    memcpy(vs.mv, d->mv, sizeof vs.mv);
    vs.p[0] = v.disp.id ? d->def_scale : 0.f;
    vs.p[1] = point_size;
    vs.p[2] = on_top ? 1.f : 0.f;
    vs.p[3] = pull;
    vs.q[0] = d->proj[5] * (float)d->vp_h;      /* pixels -> view units for the ball radius */
    vs.q[1] = v.disp2.id ? d->def_scale2 : 0.f;
    vs.q[2] = vs.q[3] = 0;
    sg_apply_uniforms(0, &SG_RANGE(vs));
    fs_params fs = {
        .color = { rgb[0], rgb[1], rgb[2], 1 },
        .rng = { d->rmin, d->rmax, (float)d->bands, (float)mode },
        .flags = { d->grey_out_of_range ? 1.f : 0.f, shade ? 1.f : 0.f, on_top ? 1.f : 0.f, 0 },
        .pz = { d->proj[10], d->proj[14], d->proj[11], d->proj[15] },
        .clip = { d->clip ? d->clip_n[0] : 0, d->clip ? d->clip_n[1] : 0, d->clip ? d->clip_n[2] : 0, d->clip ? d->clip_d : 1e30f },
    };
    sg_apply_uniforms(1, &SG_RANGE(fs));
    sg_draw(first, count, 1);
}

/* ---- auxiliary vertex sets: Gauss points, highlights, cgx geometry ------------ */

static struct { vset v; uint32_t n; } A[CV_AUX_N];
static const sg_buffer NO_IB = {0};

void cv_render_aux(int which, const float* pos, const float* disp, const float* scal, uint32_t n) {
    if (which < 0 || which >= CV_AUX_N) return;
    kill_buf(&A[which].v.pos); kill_buf(&A[which].v.disp); kill_buf(&A[which].v.scal);
    A[which].n = 0;
    if (!pos || n == 0) return;
    A[which].v.pos = make_buf(pos, (size_t)n * 12, false);
    A[which].v.disp = make_buf(disp, (size_t)n * 12, false);
    A[which].v.scal = make_buf(scal, (size_t)n * 4, false);
    A[which].n = A[which].v.pos.id ? n : 0;
}

void cv_render_draw(const cv_draw* d) {
    if (d->vp_w <= 0 || d->vp_h <= 0) return;
    sg_apply_viewport(d->vp_x, d->vp_y, d->vp_w, d->vp_h, true);
    sg_apply_scissor_rect(d->vp_x, d->vp_y, d->vp_w, d->vp_h, true);
    vset mesh = { R.pos, R.disp, R.scal, R.disp2 };
    if (d->faces)
    {
        if (d->faces_color == CV_COLOR_GROUP && R.ngroups) {
            /* one solid-colour run per group: no per-triangle lookup at all */
            for (int g = 0; g < R.ngroups; g++) {
                int first = (int)R.grp_first[g], n = (int)(R.grp_first[g + 1] - R.grp_first[g]);
                if (n > 0)
                    draw_layer(R.pip_tri, mesh, R.ib_grp, n * 3, CV_COLOR_SOLID, R.grp_rgb + 3 * g,
                               d->shade, d, 1, false, 0.f, first * 3);
            }
        } else if (d->faces_color == CV_COLOR_ELEM && A[CV_AUX_ELEMTRI].n) {
            /* flat per-element colours from the expanded triangle stream */
            draw_layer(R.pip_tri_ni, A[CV_AUX_ELEMTRI].v, NO_IB, (int)A[CV_AUX_ELEMTRI].n,
                       CV_COLOR_NODAL, d->face_rgb, d->shade, d, 1, false, 0.f, 0);
        } else {
            draw_layer(d->faces_color == CV_COLOR_ELEM ? R.pip_tri_prim : R.pip_tri, mesh, R.ib_tri,
                       (int)(R.n_tri * 3), d->faces_color == CV_COLOR_GROUP ? CV_COLOR_SOLID : d->faces_color,
                       d->face_rgb, d->shade, d, 1, false, 0.f, 0);
        }
    }
    if (d->ghost && d->def_scale != 0.f) {   /* the shape before deformation, faint */
        static const float ghost_rgb[3] = { 0.55f, 0.55f, 0.55f };
        cv_draw g = *d;
        g.def_scale = g.def_scale2 = 0.f;
        vset undeformed = { R.pos, {0}, {0}, {0} };
        draw_layer(R.pip_line, undeformed, R.ib_edge, (int)(R.n_edge * 2), CV_COLOR_SOLID, ghost_rgb, false, &g, 1, false, PULL, 0);
    }
    if (d->edges) {
        int m = d->edges_color == CV_COLOR_ELEM ? CV_COLOR_NODAL : d->edges_color;
        draw_layer(R.pip_line, mesh, R.ib_edge, (int)(R.n_edge * 2), m, d->edge_rgb, false, d, 1, false, PULL, 0);
    }
    if (d->points) {
        int m = d->points_color == CV_COLOR_ELEM ? CV_COLOR_NODAL : d->points_color;
        draw_layer(R.pip_pt, mesh, R.ib_pt, (int)R.n_pt, m, d->point_rgb, false, d, d->point_size, false, 0.f, 0);
    }
    if (d->path && A[CV_AUX_PATHLN].n) {     /* the plot line, on top */
        static const float path_rgb[3] = { 1.0f, 0.55f, 0.1f };
        draw_layer(R.pip_line_ni, A[CV_AUX_PATHLN].v, NO_IB, (int)A[CV_AUX_PATHLN].n, CV_COLOR_SOLID, path_rgb, false, d, 1, true, 0.f, 0);
    }
    if (d->markers && A[CV_AUX_MARK].n)      /* min (first) and max (second), coloured by the field */
        draw_layer(R.pip_pt_ni, A[CV_AUX_MARK].v, NO_IB, (int)A[CV_AUX_MARK].n,
                   CV_COLOR_NODAL, d->point_rgb, false, d, d->marker_size, true, 0.f, 0);
    if (d->gauss_points && A[CV_AUX_GP].n)
        draw_layer(R.pip_pt_ni, A[CV_AUX_GP].v, NO_IB, (int)A[CV_AUX_GP].n,
                   d->gauss_points_color, d->gauss_rgb, false, d, d->gauss_size, d->gauss_on_top, 0.f, 0);
    if (d->highlights) {
        /* surfaces: their faces redrawn a hair in front; node sets: bright balls */
        static const float surf_rgb[3] = { 0.93f, 0.25f, 0.75f }, nset_rgb[3] = { 1.0f, 0.55f, 0.10f };
        if (A[CV_AUX_HLTRI].n)
            draw_layer(R.pip_tri_ni, A[CV_AUX_HLTRI].v, NO_IB, (int)A[CV_AUX_HLTRI].n,
                       CV_COLOR_SOLID, surf_rgb, d->shade, d, 1, false, PULL, 0);
        if (A[CV_AUX_HLPT].n)
            draw_layer(R.pip_pt_ni, A[CV_AUX_HLPT].v, NO_IB, (int)A[CV_AUX_HLPT].n,
                       CV_COLOR_SOLID, nset_rgb, false, d, d->hl_size, false, 0.f, 0);
    }
    {
        /* supports, loads, links and springs from the deck: line glyphs with their
           true depth, so faces in front hide them; pulled a hair toward the eye
           so a glyph lying on a face wins against it */
        static const float bc_rgb[3] = { 0.15f, 0.85f, 0.85f }, ld_rgb[3] = { 1.0f, 0.78f, 0.10f };
        if (d->supports && A[CV_AUX_BCLN].n)
            draw_layer(R.pip_line_ni, A[CV_AUX_BCLN].v, NO_IB, (int)A[CV_AUX_BCLN].n,
                       CV_COLOR_SOLID, bc_rgb, false, d, 1, false, 4 * PULL, 0);
        if (d->loads && A[CV_AUX_LDLN].n)
            draw_layer(R.pip_line_ni, A[CV_AUX_LDLN].v, NO_IB, (int)A[CV_AUX_LDLN].n,
                       CV_COLOR_SOLID, ld_rgb, false, d, 1, false, 4 * PULL, 0);
        static const float vec_rgb[3] = { 0.95f, 0.95f, 0.95f };
        if (d->vectors && A[CV_AUX_VECLN].n)
            draw_layer(R.pip_line_ni, A[CV_AUX_VECLN].v, NO_IB, (int)A[CV_AUX_VECLN].n,
                       d->vectors_color, vec_rgb, false, d, 1, false, 4 * PULL, 0);
        static const float link_rgb[3] = { 0.55f, 0.95f, 0.45f };
        if (d->links && A[CV_AUX_LINKLN].n)
            draw_layer(R.pip_line_ni, A[CV_AUX_LINKLN].v, NO_IB, (int)A[CV_AUX_LINKLN].n,
                       CV_COLOR_SOLID, link_rgb, false, d, 1, false, 4 * PULL, 0);
        static const float disc_rgb[3] = { 0.80f, 0.45f, 0.95f };
        if (d->discrete && A[CV_AUX_DISCLN].n)
            draw_layer(R.pip_line_ni, A[CV_AUX_DISCLN].v, NO_IB, (int)A[CV_AUX_DISCLN].n,
                       CV_COLOR_SOLID, disc_rgb, false, d, 1, false, 4 * PULL, 0);
    }
    {
        /* cgx geometry: surfaces as patches, curves pulled onto them, points as balls */
        static const float srf_rgb[3] = { 0.60f, 0.70f, 0.84f }, crv_rgb[3] = { 0.08f, 0.16f, 0.48f },
                           pnt_rgb[3] = { 0.85f, 0.18f, 0.12f };
        if (d->geo_surfaces && A[CV_AUX_GEOTRI].n)
            draw_layer(R.pip_tri_ni, A[CV_AUX_GEOTRI].v, NO_IB, (int)A[CV_AUX_GEOTRI].n,
                       CV_COLOR_SOLID, srf_rgb, d->shade, d, 1, false, 0.f, 0);
        if (d->geo_curves && A[CV_AUX_GEOLN].n)
            draw_layer(R.pip_line_ni, A[CV_AUX_GEOLN].v, NO_IB, (int)A[CV_AUX_GEOLN].n,
                       CV_COLOR_SOLID, crv_rgb, false, d, 1, false, 4 * PULL, 0);
        if (d->geo_points && A[CV_AUX_GEOPT].n)
            draw_layer(R.pip_pt_ni, A[CV_AUX_GEOPT].v, NO_IB, (int)A[CV_AUX_GEOPT].n,
                       CV_COLOR_SOLID, pnt_rgb, false, d, d->geo_size, false, 0.f, 0);
    }
}

static void clear_aux(void) {
    for (int i = 0; i < CV_AUX_N; i++) cv_render_aux(i, NULL, NULL, NULL, 0);
}