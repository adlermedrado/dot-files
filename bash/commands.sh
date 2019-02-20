c() { 
    cd ~/devel/$* && pyenv activate $*;
}

dp() {
    pyenv deactivate
}

ctp() {
    ctags -V -R --fields=+l --languages=python --python-kinds=-iv -f ./tags ./ $(python -c "import os, sys; print(' '.join('{}'.format(d) for d in sys.path if os.path.isdir(d)))")
}
