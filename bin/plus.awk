# push 2 literals
# plus, T <- T + N, N <- first literal
/dsp=02 .* T=700f N=0ff0/,/dsp=02 .* T=7011 N=0ff0/ {
    print
}
