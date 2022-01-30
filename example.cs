class Hello
{
    int ga;
    char b;
    
    void main()
    {
        int b;
        ga = 6;
        print(square(ga));
    }
    
    int square( int x )
    {
        int y;
        y = x*x;
        return y;   
    }

    int abs(int x)
    {
    	
        if (x<0)
            x = 0-x;
        return x;
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
