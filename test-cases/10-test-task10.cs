class Hello
{
    void main()
    {
      print (fac(10));
    }

    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
    }
}