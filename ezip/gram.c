/* system calls for graphics */

setmode (n) int n; {
    return(n);
}

getmode () {
    return(1);
}

wdot(x, y, attr) int x, y, attr; {
    return(x + y + attr);
}