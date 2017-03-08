struct pack { int x; int y; int z; };

struct pack structpack(struct pack p){
    struct pack r;
    r.x = 0;
    r.y = 0;
    r.z = 0;
    struct pack t = { .x = p.x + 1, .y = p.y + 1, .z = p.z + 1 };
    r = t;
    return r;
}
