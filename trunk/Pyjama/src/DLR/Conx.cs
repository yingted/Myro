using System;
using System.Text;
using System.Collections.Generic;
using System.IO;
using System.Collections;


public class Functions
{
    public static Dictionary<string,double[]> sumMerge(Dictionary<string,double[]> dict1,
        Dictionary<string,double[]> dict2)
    {
        foreach(string key in dict2.Keys)
        {
            dict1.Add(key, dict2[key]);
        }
        return dict1;
    }

    public static double[] copyDoubleArray(double[] array)
    {
        double[] array2 = (double[]) array.Clone();
        return array2;
    }

    public static double[][] copyDoubleMatrix(double[][] matrix)
    {

        double[][] matrix2 = (double[][]) matrix;
        return matrix2;
    }
}





public class Activation
{
    public double minTarget  = 0;
    public double minActivation = 0;
    public double maxTarget = 1;
    public double maxActivation = 1;
    public double sigmoid_prime_offset;
    
    public Activation(double sigmoid_prime_offset)
    {
        this.sigmoid_prime_offset = sigmoid_prime_offset;
    }

    public virtual double[] activationFunction(double[] array)
    {return array;}
    
    public virtual double actPrime(double value)
    { return value;}
    
    public virtual double actDeriv(double value)
    { return this.actPrime(value);}

}


public class ActASIG: Activation
{
    public ActASIG(double offset): base(offset)
    {}
    
    public override double[] activationFunction(double[] array)
    {
        double[] array2 = new double[array.Length];
        for(int i=0; i<array.Length; i++)
        {
            if (array[i] < -15.0) { array2[i]=0.0;}
            else
            {
                if (array[i]>15.0) { array2[i]=1.0;}
                else { array2[i]= (1.0/(1.0+Math.Exp(-array[i])));}
            }
        }
        return array2;
    }
    
    public override double actPrime(double value)
    {
        double retval = (value*(1.0-value))+this.sigmoid_prime_offset;
        return retval;
    }
    
    public override double actDeriv(double value)
    {
        if(value<-15.0){value = 0.0;}
        else
        {
            if(value>15.0){value=1.0;}
            else {value = (1.0/(1.0+Math.Exp(-value)));}
        }
        value = value*(1.0-value)+this.sigmoid_prime_offset;
        return value;
    }
}


public class ActFahlman: Activation
{
    public ActFahlman(double offset): base(offset)
    {
        this.minActivation= -.5;
        this.minTarget = -.5;
        this.maxActivation = .5;
        this.maxTarget = .5;
    }

    public override double[] activationFunction(double[] array)
    {
        double[] array2 = new double[array.Length];
        for(int i=0; i<array.Length; i++)
        {
            if (array[i] < -15.0) { array2[i]=-0.5;}
            else
            {
                if (array[i]>15.0) { array2[i]=0.5;}
                else { array2[i]= (1.0/(1.0+Math.Exp(-array[i]))) - 0.5;}
            }
        }
        return array2;
    }
    
    public override double actPrime(double value)
    {
        double retval = - value*value+this.sigmoid_prime_offset + .25;
        return retval;
    }

    public override double actDeriv(double value)
    {
        if (value < -15.0) { value =-0.5;}
            else
            {
                if (value>15.0) {value=0.5;}
                else { value = (1.0/(1.0+Math.Exp(-value))) - 0.5;}
            }
        return this.actPrime(value);
    }
}


public class ActTANH: Activation
{
    public ActTANH(double offset): base(offset)
    {
        this.minActivation= -1.7159;
        this.minTarget = -1.7159;
        this.maxActivation = 1.7159;
        this.maxTarget = 1.7159;
    }

    public override double[] activationFunction(double[] array)
    {
        double[] array2 = new double[array.Length];
        for(int i=0; i<array.Length; i++)
        {
            if (array[i] < -15.0) { array2[i]=-1.0;}
            else
            {
                if (array[i]>15.0) { array2[i]=1.0;}
                else { array2[i]= 1.7159*Math.Tanh(0.66666666666*array[i]);}
            }
        }
        return array2;
    }
    
    public override double actPrime(double value)
    {
        value = 1.0 - value*value + this.sigmoid_prime_offset;
        return value;
    }
    
    public override double actDeriv(double value)
    {
        if (value < -15.0) { value =-1.0;}
        else
            {
                if (value>15.0) { value =1.0;}
                else { value = 1.7159*Math.Tanh(0.66666666666*value);}
            }
        return this.actPrime(value);
    }
}
    
    
    
    
    
    
    
public class Node
{
    public Layer layer;
    public int position;
    public double error;
    public double target;
    public double netinput;
    public double weight;
    public double activation;

    public Node(Layer layer, int position)
    {
        this.layer = layer;
        this.position = position;
    }

    public Node(Layer layer, int position, double activation, double error, double target, double netinput, double weight)
    {
        this.layer = layer;
        this.position = position;
        this.activation = activation;
        this.error = error;
        this.target = target;
        this.netinput = netinput;
        this.weight = weight;
    }

    public void display()
    {
        Console.WriteLine("Node:");
        Console.WriteLine("   layer = {0}", this.layer.name);
        Console.WriteLine("   position = {0}", this.position);
        try
        {
            Console.WriteLine("   weight = {0}", this.weight);
            Console.WriteLine("   activation = {0}", this.activation);
            Console.WriteLine("   error = {0}", this.error);
            Console.WriteLine("   target = {0}", this.target);
            Console.WriteLine("   netinput = {0}", this.netinput);

        }
        catch
        {
            Console.WriteLine("No node values initialized yet.");
        }

    }

public void update()
    {
        try
        {
            int pos = this.position;
            this.activation = this.layer.activation[pos];
            this.error = this.layer.error[pos];
            this.target = this.layer.target[pos];
            this.netinput = this.layer.netinput[pos];
            this.weight = this.layer.weight[pos];
        }
        catch
        {
            Exception e = new Exception("Problem getting vlues from layer.");
            throw e;
        }
    }
}








public class Layer
{
    public string name;
    public int size;
    public int displayWidth;
    public string type="Undefined";
    public string kind="Undefined";
    public int verbosity =0;
    //public int log;
    //public string logFile;
    //public int logPtr;
    public bool active = true;
    public bool frozen=false;
    public double maxRandom = .1;
    public bool verify = true;
    public double pcorrect = 0.0;
    public double ptotal = 0.0;
    public double correct =0;
    public double minTarget=0;
    public double maxTarget=1; 
    public double minActivation=0;
    public double maxActivation=1;
    public double[] dweight;
    public double[] delta;
    public double[] wed;
    public double[] wedLast;
    public double[] target;
    public double[] error;
    public double[] activation;
    public double[] netinput;
    public double[] weight;
    public bool targetSet=false;
    public bool activationSet=false;
    public bool patternReport = false;



    public Layer(string name, int size)
    {
        
        if (size <= 0)
        {
            Exception e = new Exception("Layer was initialized with size zero.");
            throw e;
        }
        

        //this.log = 0;
        //this.logFile = "";
        //this._logPtr = 0;
        this.size = size;
        this.name = name;
        this.initialize();
    }

    public Layer(string name, int size, double maxRandom)
    {

        if (size <= 0)
        {
            Exception e = new Exception("Layer was initialized with size zero.");
            throw e;
        }

        //this.log = 0;
        //this.logFile = "";
        //this._logPtr = 0;

        this.maxRandom = maxRandom;
        this.name = name;
        this.size=size;
        this.initialize();
    }

    public void initialize()
    {
        int size = this.size;
        this.randomize(1);
        this.dweight = new double[size];
        this.dweight.Initialize();
        this.delta = new double[size];
        this.delta.Initialize();
        this.wed = new double[size];
        this.wed.Initialize();
        this.wedLast = new double[size];
        this.wedLast.Initialize();
        this.target = new double[size];
        this.target.Initialize();
        this.error = new double[size];
        this.error.Initialize();
        this.activation = new double[size];
        this.activation.Initialize();
        this.netinput = new double[size];
        this.netinput.Initialize();
        this.targetSet = false;
        this.activationSet = false;
    }


    public void randomize(int force)
    {
        if ((!this.frozen)||(force != 0))
        {
            int sign;
            double temp;
            double number;
            this.weight = new double[this.size];
            for(int i=0; i<this.size; i++)
            {
                sign = Network.random.Next(0,2);
                temp = Network.random.NextDouble();
                if (sign==1)
                {
                    number = temp*this.maxRandom;
                }
                else
                {
                    number = -temp*this.maxRandom;
                }
                this.weight[i] = number;
            }
        }
    }

    public Node makeNode(int i)
    {
        try
        {
            Node n = new Node(this, i, this.activation[i], this.error[i], this.target[i],
                      this.netinput[i], this.weight[i]);
            return n;
        }
        catch
        {
            Exception e = new Exception("Cannot generate node at this position.");
            throw e;
        }
    }

    public void setActive(bool value)
    {
        this.active = value;
    }

    public bool getActive()
    {
        return this.active;
    }

    public void changeSize(int newsize)
    {
        if (newsize <= 0)
        {
            Exception e = new Exception("Layer size invalid.");
            throw e;
        }
        int minSize = Math.Min(newsize, this.size);
        int sign;
        double temp;
        double number;
        double[] bias = new double[newsize];
        for(int i=0; i<minSize; i++)
        {
            bias[i]=this.weight[i];
        }
        for(int i=minSize; i<newsize; i++)
        {
            sign = Network.random.Next(0,2);
            temp = Network.random.NextDouble();
            if (sign==1)
            {
                number = temp*this.maxRandom;
            }
            else
            {
                number = -temp*this.maxRandom;
            }
            bias[i] = number;
        }
        this.weight = bias;
        this.size = newsize;
        this.targetSet = false;
        this.displayWidth = newsize;
        this.activationSet = false;
        this.target = new double[newsize];
        this.target.Initialize();
        this.error = new double[newsize];
        this.error.Initialize();
        this.activation = new double[newsize];
        this.activation.Initialize();
        this.dweight = new double[newsize];
        this.dweight.Initialize();
        this.delta = new double[newsize];
        this.delta.Initialize();
        this.netinput = new double[newsize];
        this.netinput.Initialize();
        this.wed = new double[newsize];
        this.wed.Initialize();
        this.wedLast = new double[newsize];
        this.wedLast.Initialize();
    }

    public double TSSError()
    {
        double error = 0;
        for(int i=0;i < this.size; i++)
        {
            error += Math.Pow((this.target[i]-this.activation[i]),2);
        }
        return error;
    }

    public double RMSError()
    {
        double tss = this.TSSError();
        return Math.Sqrt(tss/(double) this.size);
    }

    public int getCorrect(double tolerance)
    {
        int count = 0;
        for(int i=0; i < this.size; i++)
        {
            if (Math.Abs(this.target[i]-this.activation[i])<tolerance)
            {
                count+=1;
            }
        }
        return count;
    }

    public double[] getWinner(string type)
    {
        double maxvalue = -10000;
        double maxpos = -1;
        double ttlvalue = 0;
        if (type == "activation")
        {
            foreach (double act in this.activation)
            {
                ttlvalue += act;
                if (act > maxvalue)
                {
                    maxvalue = act;
                }
            }
            maxpos = Array.IndexOf(this.activation, maxvalue);
        }
        else
        {
            if(type == "target")
            {
                if (this.verify && !this.targetSet)
                {
                    Exception e = new Exception("getWinner() called but target has not been set.");
                    throw e;
                }
                foreach (double act in this.target)
                {
                    ttlvalue += act;
                    if (act > maxvalue)
                    {
                        maxvalue = act;
                    }
                }
                maxpos = Array.IndexOf(this.activation, maxvalue);
            }
            else
            {
                Exception e = new Exception("getWinner() called with unknown Layer Atttribute.");
                throw e;
            }
        }
        double avgvalue;
        if (this.size > 0)
        {
            avgvalue = ttlvalue / this.size;
        }
        else
        {
            Exception e = new Exception("getWinner() called with layer of size zero.");
            throw e;
        }
        double[] values = new double[3];
        values[0]=maxpos;
        values[1]=maxvalue;
        values[2]=avgvalue;
        return values;
    }

    public void setDisplayWidth(int val)
    {
        this.displayWidth = val;
    }

    public void print(double[] array, string name)
    {
        Console.Write(name);
        Console.Write("\t");
        foreach (double tar in array)
        {
            Console.Write("{0:G4}\t", tar);
        }
        Console.Write("\n");
    }

    public void display()
    {
        Console.WriteLine("=============================");
        Console.WriteLine("Layer {0}: (Kind: {1}, Size: {2}, Active: {3}, Frozen: {4})",
                  this.name, this.kind, this.size, this.active, this.frozen);
        if (this.type == "Output")
        {
            this.print(this.target, "Target \t");
        }
        this.print(this.activation, "Activation");
        if ((this.type != "Input") && (this.verbosity > 1))
        {
            this.print(this.error, "Error \t");
        }
        if ((this.verbosity > 4) && (this.type != "Input"))
        {
            this.print(this.weight, "Weight\t");
            this.print(this.dweight, "Dweight\t");
            this.print(this.delta, "Delta \t");
            this.print(this.netinput, "NetInput\t");
            this.print(this.wed, "Wed \t");
        }
    }

    public double[] getActivations()
    {
        return this.activation;
    }

    public double[] getActivationsArray()
    {
        double[] array = new double[this.size];
        Array.Copy(this.activation, array, this.size);
        return array;
    }

    public void setActivations(double value)
    {
        for(int i=0; i<this.size; i++)
        {
            this.activation[i]=value;
        }
    }

    public void copyActivations(double[] array)
    {
        if(array.Length != this.size)
        {
            Exception e = new Exception("Mismatched activation size and layer size in call to copyActivations()");
            throw e;
        }
        //if((this.verify != 0) && (this.activationSet !=0))
        //{Exception e = new Exception("Activation flag not reset before call to copyActivations()"); throw e;}
        this.activation = array;
        this.activationSet = true;
    }

    public void copyActivations(double[] array, int reckless)
    {
        if(array.Length != this.size)
        {
            Exception e = new Exception("Mismatched activation size and layer size in call to copyActivations()");
            throw e;
        }
        if(this.verify && this.activationSet && (reckless != 0))
        {
            Exception e = new Exception("Activation flag not reset before call to copyActivations()");
            throw e;
        }
        this.activation = array;
        this.activationSet = true;
    }

    public double[] getTargetsArray()
    {
        double[] array = new double[this.size];
        Array.Copy(this.target, array, this.size);
        return array;
    }

    public double[] getTargets()
    {
        return this.target;
    }

    public void setTargets(double value)
    {
        if((value>this.maxActivation)||(value<this.minActivation))
        {
            Exception e = new Exception("Layer targets are out of the proper interval.");
            throw e;
        }
        for(int i=0; i < this.size; i++)
        {
            this.target[i]=value;
        }
        this.targetSet = true;
    }


    public void copyTargets(double[] array)
    {
        if(array.Length != this.size)
        {
            Exception e = new Exception("Mismatched activation size and layer size    in call to copyTargets()");
            throw e;
        }
        int flag = 0;
        foreach(double element in array)
        {
            if((element > this.maxTarget) || (element < this.minTarget))
            {
                flag = 1;
                break;
            }
        }
        if (flag ==1)
        {
            Exception e = new Exception("Targets for this layer out of range.");
            throw e;
        }
        this.target = array;
        this.targetSet = true;
    }


    public void resetFlags()
    {
        Console.WriteLine("Reset all flags.");
        this.targetSet = false;
        this.activationSet=false;
    }

    public void resetTargetFlag()
    {
        Console.WriteLine("Reset target flag.");
        this.targetSet = false;
    }

    public void resetActivationFlag()
    {
        Console.WriteLine("Reset activation flag.");
        this.activationSet = false;
    }

}






public class Connection
{
    public bool active = true;
    public bool frozen = false;
    public Layer fromLayer;
    public Layer toLayer;
    public double[][] dweight;
    public double[][] weight;
    public double[][] wed;
    public double[][] wedLast;

    public Connection(Layer fromLayer, Layer toLayer)
    {
        this.fromLayer = fromLayer;
        this.toLayer = toLayer;
        this.initialize();
    }

    //public double getWeight(int i)
    //{return this.weight[i];}

    public void initialize()
    {
        int fromLayerSize=this.fromLayer.size;
        int toLayerSize=this.toLayer.size;
        double[][] dweight = new double[fromLayerSize][];
        double[][] wed = new double[fromLayerSize][];
        double[][] wedLast = new double[fromLayerSize][];
        double[][] weight = new double[fromLayerSize][];
        for(int i=0;i<fromLayerSize;i++)
        {
            dweight[i]= new double[toLayerSize];
            wed[i]= new double[toLayerSize];
            wedLast[i] = new double[toLayerSize];
            weight[i] = new double[toLayerSize];
        }
        this.dweight = dweight;
        this.weight = weight;
        this.wed = wed;
        this.wedLast = wedLast;
        this.weight.Initialize();
        this.dweight.Initialize();
        this.wed.Initialize();
        this.wedLast.Initialize();
        this.randomize(0);
    }

    public void randomize(int force)
    {
        if ((!this.frozen)||(force != 0))
        {
            int sign;
            double temp;
            double number;
            for(int i=0; i<this.fromLayer.size; i++)
            {
                for(int j=0; j < this.toLayer.size; j++)
                {
                    sign = Network.random.Next(0,2);
                    temp = Network.random.NextDouble();
                    if (sign==1)
                    {
                        number = temp*this.toLayer.maxRandom;
                    }
                    else
                    {
                        number = -temp*this.toLayer.maxRandom;
                    }
                    this.weight[i][j] = number;
                }
            }
        }
    }



    public void changeSize(int fromLayerSize, int toLayerSize)
    {
        if((toLayerSize<1)||(fromLayerSize<1))
        {
            Exception e = new Exception("changeSize() called with invalid layer size.");
            throw e;
        }
        double[][] dweight = new double[fromLayerSize][];
        double[][] wed = new double[fromLayerSize][];
        double[][] wedLast = new double[fromLayerSize][];
        double[][] weight = new double[fromLayerSize][];
        for(int i=0;i<fromLayerSize;i++)
        {
            dweight[i]= new double[toLayerSize];
            wed[i]= new double[toLayerSize];
            wedLast[i] = new double[toLayerSize];
            weight[i] = new double[toLayerSize];
        }
        int minFrom = Math.Min(fromLayerSize,this.fromLayer.size);
        int minTo = Math.Min(toLayerSize, this.toLayer.size);
        int sign;
        double temp;
        double number;
        for(int i=0;i<minFrom;i++)
        {
            for (int j=0; j <minTo; j++)
            {
                wed[i][j]=this.wed[i][j];
                wedLast[i][j]=this.wedLast[i][j];
                dweight[i][j]=this.dweight[i][j];
                weight[i][j]=this.weight[i][j];
            }
        }
        for(int i=minFrom; i < fromLayerSize; i++)
        {
            for(int j=minTo; j < toLayerSize; j++)
            {
                wed[i][j] = 0;
                wedLast[i][j]=0;
                dweight[i][j]=0;
                sign = Network.random.Next(0,2);
                temp = Network.random.NextDouble();
                if (sign==1)
                {
                    number = temp*this.toLayer.maxRandom;
                }
                else
                {
                    number = -temp*this.toLayer.maxRandom;
                }
                weight[i][j] = number;
            }
        }
        this.dweight = dweight;
        this.wed = wed;
        this.wedLast = wedLast;
        this.weight = weight;
    }


    public void print(double[] array, string name)
    {
        Console.Write(name);
        Console.Write("\t");
        foreach (double tar in array)
        {
            Console.Write("{0:G3}\t ", tar);
        }
        Console.Write("\n");
    }


    public void display()
    {
        if(this.toLayer.verbosity>2)
        {
            Console.WriteLine("=============================");
            Console.WriteLine("Connection from '{0}' to '{1}': (Active: {2}, Frozen: {3})",
                      this.fromLayer.name, this.toLayer.name, this.active, this.frozen);
            Console.Write("weight:");
            for(int i=0; i < this.fromLayer.size; i++)
            {
                Console.Write("\t[{0}]",i);
            }
            Console.Write("\n");
            for(int j=0; j < this.toLayer.size; j++)
            {
                Console.Write("[{0}]",j);
                for(int i=0; i < this.fromLayer.size; i++)
                {
                    Console.Write("\t{0:G2}",this.weight[i][j]);
                }
                Console.Write("\n");
            }
        }

        if(this.toLayer.verbosity>4)
        {
            Console.Write("wed:");
            for(int i=0; i < this.fromLayer.size; i++)
            {
                Console.Write("    [{0}]",i);
            }
            Console.Write("\n");
            for(int j=0; j < this.toLayer.size; j++)
            {
                Console.Write("[{0}]",j);
                for(int i=0; i < this.fromLayer.size; i++)
                {
                    Console.Write("\t{0:G2}",this.wedLast[i][j]);
                }
                Console.Write("\n");
            }
            Console.Write("dweight");
            for(int i=0; i < this.fromLayer.size; i++)
            {
                Console.Write("\t[{0}]",i);
            }
            Console.Write("\n");
            for(int j=0; j < this.toLayer.size; j++)
            {
                Console.Write("[{0}]",j);
                for(int i=0; i < this.fromLayer.size; i++)
                {
                    Console.Write("\t{0:G2}",this.dweight[i][j]);
                }
                Console.Write("\n");
            }
        }
    }
}






public class Network
{
    public static int seed;
    public static Random random = new Random();
    public List<Layer> layers = new List<Layer>();
    public int verbosity = 0;
    public bool complete = false;
    public string name = "Backprop Network";
    public Dictionary<string,Layer> layersByName = new Dictionary<string,Layer>();
    public List<Connection> connections = new List<Connection>();
    public List<List<object>> inputMap = new List<List<object>>();
    public List<List<object>> targetMap = new List<List<object>>();
    public List<string[]> association = new List<string[]>();
    public double[][] inputs;
    public double[][] targets;
    public bool orderedInputs = false;
    public int[] loadOrder;
    public bool learning = true;
    public double momentum = .9;
    public int resetEpoch = 5000;
    public int resetCount = 1;
    public int resetLimit = 1;
    public bool batch = false;
    public int epoch = 0;
    public int totalEpoch = 0;
    public int count = 0;
    public double stopPercent = 1.0;
    public double sigmoid_prime_offset = 0.1;
    public double tolerance = .4;
    public bool interactive = false;
    public double epsilon = .1;
    public int reportRate = 25;
    public int sweepReportRate = 1000;
    public List<Dictionary<string,double[]>> crossValidationCorpus = new List<Dictionary<string,double[]>>();
    public List<string> crossValidationReportLayers = new List<string>();
    public int crossValidationSampleRate = 0;
    public string crossValidationSampleFile = "dample.cv";
    public List<object> patterns = new List<object>();
    public bool patterned = false;
    public bool sharedWeights = false;
    public List<object> results = new List<object>();
    public bool autoCrossValidation = false;
    public string autoSaveWeightsFile = "";
    public string autoSaveWeightsFileFormat = "conx";
    public string lastAutoSaveFile = "";
    public string lastAutoSaveFileFormat = "conx";
    public string autoSaveNetworkFile = "";
    public string autoSaveNetworkFileFormat = "conx";
    public string lastAutoSaveNetworkFilename = "";
    public double lastLowestTSSError = Math.Pow(10,308);
    public bool cv = false;
    public bool sweeping = false;
    public double maxRandom =.1;
    public int currentSweepCount = 0;
    public bool hyperbolicError = false;
    public bool quickprop = false;
    public double mu = 1.75;
    public double splitEpsilon = 0;
    public double decay = 0;
    public List<Connection> cacheConnections = new List<Connection>();
    public List<Layer> cacheLayers = new List<Layer>();
    public bool useCrossValidationToStop = false;
    public Activation act = new ActASIG(.1);






    public static void setSeed(int seed)
    {
        Network.seed = seed;
        Network.random = new Random(seed);
        Console.WriteLine("Conx using seed: {0}", seed);
    }

    public Network()
    {
        Random r = new Random();
        seed = r.Next();
        Network.setSeed(seed);
    }

    public Network(string name)
    {
        this.name = name;
        Random r = new Random();
        seed = r.Next();
        Network.setSeed(seed);
    }

    public Network(int verbosity)
    {
        this.verbosity = verbosity;
        Random r = new Random();
        seed = r.Next();
        Network.setSeed(seed);
    }

    public Network(int verbosity, int seed)
    {
        this.verbosity = verbosity;
        Network.setSeed(seed);
    }

    public Network(string name, int verbosity, int seed)
    {
        this.verbosity = verbosity;
        Network.setSeed(seed);
        this.name = name;
    }


    public void setCache(int val)
    {
        this.cacheConnections = new List<Connection>();
        this.cacheLayers = new List<Layer>();
        if(val!=0)
        {
            foreach(Layer layer in this.layers)
            {
                if(layer.active && !layer.frozen)
                {
                    this.cacheLayers.Add(layer);
                }
            }
            foreach(Connection con in this.connections)
            {
                if(con.active && !con.frozen)
                {
                    this.cacheConnections.Add(con);
                }
            }
        }
    }


    public void setQuickprop(bool value)
    {
        if (!value)
        {
            this.quickprop = false;
            this.splitEpsilon = 0;
            this.decay = 0.000;
            this.epsilon = .1;
        }
        else
        {
            this.batch = true;
            this.quickprop = true;
            this.mu = 1.75;
            this.splitEpsilon = 1;
            this.decay = -.0001;
            this.epsilon = 4.0;
            this.name = "Quickprop Network";
        }
    }

    public bool getQuickprop()
    {
        return this.quickprop;
    }


    public int path(Layer startLayer, Layer endLayer)
    {
        Dictionary<string,Layer> next = new Dictionary<string,Layer>();
        next.Add(startLayer.name, startLayer);
        Dictionary<string,Layer> visited = new Dictionary<string,Layer>();
        while (next.Count != 0)
        {
            foreach(string key in next.Keys)
            {
                visited.Add(key, next[key]);
                next.Remove(key);
                foreach (Connection con in this.connections)
                {
                    if(con.fromLayer.name == key)
                    {
                        if(con.toLayer.name==endLayer.name)
                        {
                            return 1;
                        }
                    }
                    else
                    {
                        if(next.ContainsKey(con.toLayer.name))
                        {}
                        else
                        {
                            if(visited.ContainsKey(con.toLayer.name))
                            {}
                            else
                            {
                                next.Add(con.toLayer.name, con.toLayer);
                            }
                        }
                    }
                }
            }
        }
        return 0;
    }


    public int getLength()
    {
        return this.layers.Count;
    }

    public int getLayerIndex(Layer layer)
    {
        for(int i =0; i<this.layers.Count; i++)
        {
            if(layer==this.layers[i])
            {
                return i;
            }
        }
        return -1;
    }

    public void addLayer(string name, int size)
    {
        Layer layer = new Layer(name,size);
        layer.maxRandom = this.maxRandom;
        layer.minTarget = this.act.minTarget;
        layer.maxTarget = this.act.maxTarget;
        layer.minActivation = this.act.minActivation;
        layer.maxActivation = this.act.maxActivation;
        this.layers.Add(layer);
        this.layersByName.Add(layer.name,layer);
    }

    public void addLayer(string name, int size, int verbosity)
    {
        Layer layer = new Layer(name,size);
        layer.verbosity = verbosity;
        layer.maxRandom = this.maxRandom;
        layer.minTarget = this.act.minTarget;
        layer.maxTarget = this.act.maxTarget;
        layer.minActivation = this.act.minActivation;
        layer.maxActivation = this.act.maxActivation;
        this.layers.Add(layer);
        this.layersByName.Add(layer.name,layer);
    }

    public void addLayer(string name, int size, int verbosity, int position)
    {
        Layer layer = new Layer(name,size);
        layer.verbosity = verbosity;
        layer.maxRandom = this.maxRandom;
        layer.minTarget = this.act.minTarget;
        layer.maxTarget = this.act.maxTarget;
        layer.minActivation = this.act.minActivation;
        layer.maxActivation = this.act.maxActivation;
        this.layers.Insert(position, layer);
        this.layersByName.Add(layer.name,layer);
    }


    public int isConnected(string fromName, string toName)
    {
        foreach(Connection c in this.connections)
        {
            if ((c.fromLayer.name == fromName) && (c.toLayer.name == toName))
            {
                return 1;
            }
        }
        return 0;
    }

    public void connect(params string[] names)
    {
        for(int i =0; i<names.Length; i += 2)
        {
            string fromName = names[i];
            string toName = names[i+1];
            this.connectAt(fromName, toName);
        }
    }


    public Layer getLayer(string name)
    {
        return this.layersByName[name];
    }


    public void connectAt(string fromName, string toName)
    {
        Layer fromLayer = this.getLayer(fromName);
        Layer toLayer = this.getLayer(toName);
        if (this.getLayerIndex(fromLayer) >= this.getLayerIndex(toLayer))
        {
            Exception e = new Exception("Layers out of order.");
            throw e;
        }
        if (fromLayer.type == "Output")
        {
            fromLayer.type = "Hidden";
            fromLayer.patternReport = false;
            fromLayer.kind = "Hidden";
        }
        else
        {
            if(fromLayer.type == "Undefined")
            {
                fromLayer.type = "Input";
                fromLayer.patternReport = false;
                fromLayer.kind = "Input";
            }
        }
        if (toLayer.type == "Input")
        {
            Exception e = new Exception("Connections out of order.");
            throw e;
        }
        else
        {
            if (toLayer.type == "Undefined")
            {
                toLayer.type = "Output";
                toLayer.patternReport = true;
                toLayer.kind="Output";
            }
        }
        Connection con = new Connection(fromLayer, toLayer);
        this.connections.Add(con);
    }

    public void connectAt(string fromName, string toName, int position)
    {
        Layer fromLayer = this.getLayer(fromName);
        Layer toLayer = this.getLayer(toName);
        if (this.getLayerIndex(fromLayer) >= this.getLayerIndex(toLayer))
        {
            Exception e = new Exception("Layers out of order.");
            throw e;
        }
        if (fromLayer.type == "Output")
        {
            fromLayer.type = "Hidden";
            fromLayer.patternReport = false;
            if (fromLayer.kind == "Output")
            {
                fromLayer.kind = "Hidden";
            }
        }
        else
        {
            if(fromLayer.type == "Undefined")
            {
                fromLayer.type = "Input";
                fromLayer.patternReport = false;
                if(fromLayer.kind == "Undefined")
                {
                    fromLayer.type = "Input";
                }
            }
        }
        if (toLayer.type == "Input")
        {
            Exception e = new Exception("Connections out of order.");
            throw e;
        }
        else
        {
            if (toLayer.type == "Undefined")
            {
                toLayer.type = "Output";
                toLayer.patternReport = true;
                if( toLayer.kind == "Undefined")
                {
                    fromLayer.kind = "Output";
                }
            }
        }
        Connection con = new Connection(fromLayer, toLayer);
        this.connections.Insert(position, con);
    }

    public virtual void addThreeLayers(int inc, int hidc, int outc)
    {
        this.addLayer("Input", inc);
        this.addLayer("Hidden", hidc);
        this.addLayer("Output", outc);
        this.connect("Input", "Hidden");
        this.connect("Hidden", "Output");
    }


    public void initialize()
    {
        Console.WriteLine("Initializing {0} weights...", this.name);
        if(!this.sharedWeights)
        {
            Exception e = new Exception("Shared weights broken.");
            throw e;
        }
        this.count = 0;
        foreach(Connection con in this.connections)
        {
            con.initialize();
        }
        foreach(Layer layer in this.layers)
        {
            layer.initialize();
        }
    }


    public void resetFlags()
    {
        foreach(Layer layer in this.layers)
        {
            layer.resetFlags();
        }
    }

    public void putActivations(Dictionary<string,double[]> dict)
    {
        foreach(string name in dict.Keys)
        {
            this.layersByName[name].copyActivations(dict[name]);
        }
    }

    public Dictionary<string,object> getActivationsDict(List<string> nameList)
    {
        Dictionary<string,object> retval = new Dictionary<string,object>();
        foreach(string name in nameList)
        {
            retval.Add(name, this.layersByName[name].getActivationsArray());
        }
        return retval;
    }


    public void setLayerVerification(bool value)
    {
        foreach (Layer layer in this.layers)
        {
            layer.verify = value;
        }
    }

    public void setEpsilon(double value)
    {
        this.epsilon = value;
    }

    public void setInteractive(bool value)
    {
        this.interactive = value;
    }

    public void setTolerance(double value)
    {
        this.tolerance = value;
    }

    public void setActive(string layerName, bool value)
    {
        this.getLayer(layerName).setActive(value);
    }

    public bool getActive(string layerName)
    {
        return this.getLayer(layerName).getActive();
    }

    public void setLearning(bool value)
    {
        this.learning = value;
    }

    public void setMomentum(double value)
    {
        this.momentum = value;
    }

    public void setResetLimit(int value)
    {
        this.resetLimit = value;
    }

    public void setResetEpoch(int value)
    {
        this.resetEpoch = value;
    }

    public void setBatch(bool value)
    {
        this.batch = value;
    }

    public Connection getConnection(string lfrom, string lto)
    {
        foreach(Connection con in this.connections)
        {
            if((con.fromLayer.name == lfrom)&&(con.toLayer.name==lto))
            {
                return con;
            }
        }
        Exception e = new Exception("Connection was not found.");
        throw e;
    }


    public void setVerbosity(int value)
    {
        this.verbosity = value;
        foreach(Layer layer in this.layers)
        {
            layer.verbosity = value;
        }
    }

    public int getVerbosity()
    {
        return this.verbosity;
    }

    public void setStopPercent(double value)
    {
        this.stopPercent = value;
    }

    public void setUseCrossValidationToStop(bool value)
    {
        this.useCrossValidationToStop = value;
    }

    public void setSigmoid_prime_offset(int value)
    {
        this.sigmoid_prime_offset = value;
    }

    public void setReportRate(int value)
    {
        this.reportRate = value;
    }

    public void setSweepReportRate(int value)
    {
        this.sweepReportRate = value;
    }

    public double getMaxRandom()
    {
        return this.maxRandom;
    }

    public void setMaxRandom(double value)
    {
        this.maxRandom = value;
        foreach(Layer layer in this.layers)
        {
            layer.maxRandom = value;
        }
    }

    public double[][] getWeights(string fromName, string toName)
    {
        foreach(Connection con in this.connections)
        {
            if((con.fromLayer.name==fromName)&&(con.toLayer.name==toName))
            {
                return con.weight;
            }
        }
        Exception e = new Exception("Connection was not found.");
        throw e;
    }

    public double setWeight(string fromName, int fromPos, string toName, int toPos, double value)
    {
        foreach(Connection con in this.connections)
        {
            if((con.fromLayer.name==fromName)&&(con.toLayer.name==toName))
            {
                con.weight[fromPos][toPos] = value;
            }
        }
        Exception e = new Exception("Connection was not found.");
        throw e;
    }

    public void setOrderedInputs(bool value)
    {
        this.orderedInputs = value;
        if (this.orderedInputs)
        {
            this.loadOrder = new int[this.inputs.Length];
            for(int i = 0; i< this.inputs.Length; i++)
            {
                this.loadOrder[i]=i;
            }
        }
    }


    public void setInputs(double[][] inputs)
    {
        //if(!this.verifyArguments(inputs)&&!this.patterned){raise Exception}
        this.inputs = inputs;
        this.loadOrder = new int[inputs.Length];
        for(int i = 0; i< this.inputs.Length; i++)
        {
            this.loadOrder[i]=i;
        }
    }

    public void setOutputs(double[][] outputs)
    {
        this.setTargets(outputs);
    }

    public void setTargets(double[][] targets)
    {
        //if(!this.verifyArguments(inputs) && !this.patterned){raise Exception}
        this.targets = targets;
    }


    public void associate(string inName, string outName)
    {
        string[] list = new string[2];
        list[0]=inName;
        list[2]=outName;
        this.association.Add(list);
    }

    public void mapInput(string layerName, int offset)
    {
        List<object> list = new List<object>();
        list.Add(layerName);
        list.Add(offset);
        this.inputMap.Add(list);
    }

    public void mapInputs(List<object> offsetPairs)
    {
        foreach(List<object> pair in offsetPairs)
        {
            this.mapInput((string) pair[0], (int) pair[1]);
        }
    }

    public void mapTarget(string layerName, int offset)
    {
        List<object> list = new List<object>();
        list.Add(layerName);
        list.Add(offset);
        this.targetMap.Add(list);
    }

    public void mapTargets(List<object> offsetPairs)
    {
        foreach(List<object> pair in offsetPairs)
        {
            this.mapTarget((string) pair[0], (int) pair[1]);
        }
    }


    public void verifyInputs()
    {
        foreach(Layer layer in this.layers)
        {
            if(layer.verify&&(layer.type=="Input")&&layer.active&&!layer.activationSet)
            {
                Exception e = new Exception("Inputs are not set and verifyInputs() was called.");
                throw e;
            }
            else
            {
                layer.resetActivationFlag();
            }
        }
    }

    public void verifyTargets()
    {
        //foreach(Layer layer in this.layers)
        //    {
        //    Console.WriteLine("{0}, {1}, {2}, {3}", layer.verify, layer.type, layer.active, layer.targetSet);
        //    if((layer.verify==1)&&(layer.type=="Output")&&(layer.active==1)&&(layer.targetSet==0))
        //    {Exception e = new Exception("Targets are not set and verifyTargets() was called."); throw e;}
        //    else {}
        //    }
    }


    public double getCorrect(string layerName)
    {
        return this.getLayer(layerName).getCorrect(this.tolerance);
    }

    public double RMSError()
    {
        double tss = 0.0;
        int size = 0;
        foreach(Layer layer in this.layers)
        {
            if(layer.type == "Output")
            {
                tss += layer.TSSError();
                size += layer.size;
            }
        }
        return Math.Sqrt(tss/size);
    }

    public double TSSError(string layerName)
    {
        return this.getLayer(layerName).TSSError();
    }


    public void reportEpoch(int epoch, double tssErr, int totalCorrect, int totalCount, double rmsErr)
    {
        Console.WriteLine("Epoch #{0,-6} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                  epoch, tssErr, totalCorrect*1.0/totalCount, rmsErr);
    }


    public void reportEpoch(int epoch, double tssErr, int totalCorrect, int totalCount, double rmsErr,
                Dictionary<string,double[]> pcorrect)
    {
        Console.WriteLine("Epoch #{0,-6} | TSS Error: {1:F4} | Correct: {2:F4} | RMS Error: {3:F4}",
                  epoch, tssErr, totalCorrect*1.0 /totalCount, rmsErr);

        foreach(string layerName in pcorrect.Keys)
        {
            if((this.layersByName[layerName].active)&&(this.layersByName[layerName].size>0))

            {
                Console.WriteLine("   Epoch #{0,-6} | Layer = {1,-12} | Units: {2:F4} | Patterns: {3:F4}",
                          epoch, layerName, pcorrect[layerName][0]/pcorrect[layerName][1],
                          pcorrect[layerName][2]/pcorrect[layerName][3]);
            }
            this.layersByName[layerName].pcorrect = pcorrect[layerName][2];
            this.layersByName[layerName].ptotal = pcorrect[layerName][2];
            this.layersByName[layerName].correct = pcorrect[layerName][2]/pcorrect[layerName][2];
        }
    }



    public void reportFinal(int epoch, double tssErr, int totalCorrect, int totalCount, double rmsErr)
    {
        Console.WriteLine("Final #{0,-6} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                  epoch, tssErr, totalCorrect*1.0 /totalCount, rmsErr);
    }

    public void reportFinal(int epoch, double tssErr, int totalCorrect, int totalCount, double rmsErr,
                Dictionary<string,double[]> pcorrect)
    {
        Console.WriteLine("Final #{0,-6} | TSS Error: {1:F4} | Correct: {2:F4} | RMS Error: {3:F4}",
                  epoch, tssErr, totalCorrect*1.0/totalCount, rmsErr);

        foreach(string layerName in pcorrect.Keys)
        {
            if((this.layersByName[layerName].active)&&(this.layersByName[layerName].size>0))
            {
                Console.WriteLine("   Final #{0,-6} | Layer = {1,-12} | Units: {2:F4} | Patterns: {3:F4}",
                          epoch, layerName, (double) pcorrect[layerName][0]/pcorrect[layerName][1],
                          (double) pcorrect[layerName][2]/pcorrect[layerName][3]);
            }
            this.layersByName[layerName].pcorrect = pcorrect[layerName][2];
            this.layersByName[layerName].ptotal = pcorrect[layerName][2];
            this.layersByName[layerName].correct = (double) pcorrect[layerName][2]/pcorrect[layerName][2];
        }
    }




    public bool doWhile(int totalCount, int totalCorrect)
    {
        if ((totalCount != 0) && ((totalCorrect*1.0 /totalCount < this.stopPercent)||
                      (this.useCrossValidationToStop)))
        {
            return true;
        }
        else
        {
            return false;
        }
    }




    public void train()
    {
        this.complete = false;
        double tssErr = 0.0;
        double rmsErr = 0.0;
        int totalCorrect = 0;
        int totalCount = 1;
        Dictionary<string,double[]> totalPCorrect = new Dictionary<string,double[]>();
        ;
        this.epoch = 1;
        this.reportStart();
        this.resetCount=1;
        this.lastLowestTSSError = Math.Pow(10,308);
        while(this.doWhile(totalCount,totalCorrect))
        {
            List<object> list = this.sweep();
            tssErr = (double) list[0];
            totalCorrect = (int) list[1];
            totalCount = (int) list[2];
            totalPCorrect = (Dictionary<string,double[]>) list[3];
            Console.WriteLine("Correct: {0}, Count: {1}, Length of Dictionary: {2}", totalCorrect, totalCount, totalPCorrect.Count);
            if (totalCount!= 0)
            {
                rmsErr = Math.Sqrt(tssErr/totalCount);
            }
            else
            {
                Console.WriteLine("Warning: sweep didn't do anything.");
            }
            if (this.epoch%this.reportRate == 0)
            {
                this.reportEpoch(this.epoch, tssErr, totalCorrect, totalCount, rmsErr, totalPCorrect);
                if ((this.autoCrossValidation)||(this.crossValidationCorpus.Count>0))
                {
                    List<object> retval = this.sweepCrossValidation();
                    double tssCVErr = (double) retval[0];
                    int totalCVCorrect = (int) retval[1];
                    int totalCVCount = (int) retval[2];
                    double rmsCVErr = Math.Sqrt(tssCVErr/totalCVCount);
                    Console.WriteLine("CV #{0,-9} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                              this.epoch, tssCVErr, (double) totalCVCorrect/totalCVCount, rmsCVErr);
                    if((this.autoSaveWeightsFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                    {
                        this.lastLowestTSSError = tssCVErr;
                        this.saveWeightsToFile(this.autoSaveWeightsFile);
                        Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveWeightsFile);
                    }
                    if((this.autoSaveNetworkFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                    {
                        this.lastLowestTSSError = tssCVErr;
                        this.saveNetworkToFile(this.autoSaveNetworkFile);
                        Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveNetworkFile);
                    }
                    if(((double) totalCVCorrect/totalCVCount >= this.stopPercent)&&(this.useCrossValidationToStop))
                    {
                        this.epoch++;
                        break;
                    }
                }
            }
            if (this.resetEpoch == this.epoch)
            {
                if(this.resetLimit==this.resetCount)
                {
                    Console.WriteLine("Reset limit reached; ending without reaching goal.");
                    this.complete = false;
                    break;
                }
                this.resetCount++;
                Console.WriteLine("Reset! resetEpoch reached; starting over...");
                this.initialize();
                tssErr = 0.0;
                rmsErr = 0.0;
                this.epoch = 1;
                totalCorrect = 0;
                totalPCorrect = new Dictionary<string,double[]>();
                continue;
            }
            this.epoch ++;
        }
        if(totalCount > 0)
        {
            this.reportFinal(this.epoch-1, tssErr, totalCorrect, totalCount, rmsErr, totalPCorrect);
            if ((this.autoCrossValidation)||(this.crossValidationCorpus.Count>0))
            {
                List<object> retval = this.sweepCrossValidation();
                double tssCVErr = (double) retval[0];
                int totalCVCorrect = (int) retval[1];
                int totalCVCount = (int) retval[2];
                double rmsCVErr = Math.Sqrt(tssCVErr/totalCVCount);
                Console.WriteLine("CV #{0,-9} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                          this.epoch, tssCVErr, (double) totalCVCorrect/totalCVCount, rmsCVErr);
                if((this.autoSaveWeightsFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                {
                    this.lastLowestTSSError = tssCVErr;
                    this.saveWeightsToFile(this.autoSaveWeightsFile);
                    Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveWeightsFile);
                }
                if((this.autoSaveNetworkFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                {
                    this.lastLowestTSSError = tssCVErr;
                    this.saveNetworkToFile(this.autoSaveNetworkFile);
                    Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveNetworkFile);
                }
            }
        }
        else
        {
            Console.WriteLine("Final: nothing done.");
        }
    }


    public void train(int cont)

    {
        this.complete = false;
        double tssErr = 0.0;
        double rmsErr =0.0;
        int totalCorrect = 0;
        int totalCount = 1;
        Dictionary<string,double[]> totalPCorrect = new Dictionary<string,double[]>();
        if(cont==0)
        {
            this.epoch = 1;
            this.reportStart();
            this.resetCount=1;
            this.lastLowestTSSError = Math.Pow(10,308);
        }
        while(this.doWhile(totalCount,totalCorrect))
        {
            List<object> list = this.sweep();
            tssErr = (double) list[0];
            totalCorrect = (int) list[1];
            totalCount = (int) list[2];
            totalPCorrect = (Dictionary<string,double[]>) list[3];
            if (totalCount!= 0)
            {
                rmsErr = Math.Sqrt(tssErr/totalCount);
            }
            else
            {
                Console.WriteLine("Warning: sweep didn't do anything.");
            }
            if (this.epoch%this.reportRate == 0)
            {
                this.reportEpoch(this.epoch, tssErr, totalCorrect, totalCount, rmsErr, totalPCorrect);
                if ((this.autoCrossValidation)||(this.crossValidationCorpus.Count>0))
                {
                    List<object> retval = this.sweepCrossValidation();
                    double tssCVErr = (double) retval[0];
                    int totalCVCorrect = (int) retval[1];
                    int totalCVCount = (int) retval[2];
                    double rmsCVErr = Math.Sqrt(tssCVErr/totalCVCount);
                    Console.WriteLine("CV #{0,-9} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                              this.epoch, tssCVErr, (double) totalCVCorrect/totalCVCount, rmsCVErr);
                    if((this.autoSaveWeightsFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                    {
                        this.lastLowestTSSError = tssCVErr;
                        this.saveWeightsToFile(this.autoSaveWeightsFile);
                        Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveWeightsFile);
                    }
                    if((this.autoSaveNetworkFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                    {
                        this.lastLowestTSSError = tssCVErr;
                        this.saveNetworkToFile(this.autoSaveNetworkFile);
                        Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveNetworkFile);
                    }
                    if(((double) totalCVCorrect/totalCVCount >= this.stopPercent)&&(this.useCrossValidationToStop))
                    {
                        this.epoch++;
                        break;
                    }
                }
            }
            if (this.resetEpoch == this.epoch)
            {
                if(this.resetCount==this.resetLimit)
                {
                    Console.WriteLine("Reset limit reached; ending without reaching goal.");
                    this.complete = false;
                    break;
                }
                this.resetCount++;
                Console.WriteLine("Reset! resetEpoch reached; starting over...");
                this.initialize();
                tssErr = 0.0;
                rmsErr = 0.0;
                this.epoch = 1;
                totalCorrect = 0;
                totalPCorrect = new Dictionary<string,double[]>();
                continue;
            }
            this.epoch ++;
        }
        if(totalCount > 0)
        {
            this.reportFinal(this.epoch-1, tssErr, totalCorrect, totalCount, rmsErr, totalPCorrect);
            if ((this.autoCrossValidation)||(this.crossValidationCorpus.Count>0))
            {
                List<object> retval = this.sweepCrossValidation();
                double tssCVErr = (double) retval[0];
                int totalCVCorrect = (int) retval[1];
                int totalCVCount = (int) retval[2];
                double rmsCVErr = Math.Sqrt(tssCVErr/totalCVCount);
                Console.WriteLine("CV #{0,-9} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                          this.epoch, tssCVErr, (double) totalCVCorrect/totalCVCount, rmsCVErr);
                if((this.autoSaveWeightsFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                {
                    this.lastLowestTSSError = tssCVErr;
                    this.saveWeightsToFile(this.autoSaveWeightsFile);
                    Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveWeightsFile);
                }
                if((this.autoSaveNetworkFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                {
                    this.lastLowestTSSError = tssCVErr;
                    this.saveNetworkToFile(this.autoSaveNetworkFile);
                    Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveNetworkFile);
                }
            }
        }
        else
        {
            Console.WriteLine("Final: nothing done.");
        }
    }


    public void train(int cont, int sweeps)

    {

        this.complete = false;
        double tssErr = 0.0;
        double rmsErr =0.0;
        int totalCorrect = 0;
        int totalCount = 1;
        Dictionary<string,double[]> totalPCorrect = new Dictionary<string,double[]>();
        if(cont==0)
        {
            this.epoch = 1;
            this.reportStart();
            this.resetCount=1;
            this.lastLowestTSSError = Math.Pow(10,308);
            if (sweeps>0)
            {
                this.resetEpoch=sweeps;
            }
        }
        else
        {
            if(sweeps>0)
            {
                this.resetEpoch = this.epoch + sweeps - 1;
            }
        }
        while(this.doWhile(totalCount,totalCorrect))
        {
            List<object> list = this.sweep();
            tssErr = (double) list[0];
            totalCorrect = (int) list[1];
            totalCount = (int) list[2];
            totalPCorrect = (Dictionary<string,double[]>) list[3];
            if (totalCount!= 0)
            {
                rmsErr = Math.Sqrt(tssErr/totalCount);
            }
            else
            {
                Console.WriteLine("Warning: sweep didn't do anything.");
            }
            if (this.epoch%this.reportRate == 0)
            {
                this.reportEpoch(this.epoch, tssErr, totalCorrect, totalCount, rmsErr, totalPCorrect);
                if ((this.autoCrossValidation)||(this.crossValidationCorpus.Count>0))
                {
                    List<object> retval = this.sweepCrossValidation();
                    double tssCVErr = (double) retval[0];
                    int totalCVCorrect = (int) retval[1];
                    int totalCVCount = (int) retval[2];
                    double rmsCVErr = Math.Sqrt(tssCVErr/totalCVCount);
                    Console.WriteLine("CV #{0,-9} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                              this.epoch, tssCVErr, (double) totalCVCorrect/totalCVCount, rmsCVErr);
                    if((this.autoSaveWeightsFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                    {
                        this.lastLowestTSSError = tssCVErr;
                        this.saveWeightsToFile(this.autoSaveWeightsFile);
                        Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveWeightsFile);
                    }
                    if((this.autoSaveNetworkFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                    {
                        this.lastLowestTSSError = tssCVErr;
                        this.saveNetworkToFile(this.autoSaveNetworkFile);
                        Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveNetworkFile);
                    }
                    if(((double) totalCVCorrect/totalCVCount >= this.stopPercent)&&(this.useCrossValidationToStop))
                    {
                        this.epoch++;
                        break;
                    }
                }
            }
            if (this.resetEpoch == this.epoch)
            {
                if(this.resetCount==this.resetLimit)
                {
                    Console.WriteLine("Reset limit reached; ending without reaching goal.");
                    this.complete = false;
                    break;
                }
                this.resetCount++;
                Console.WriteLine("Reset! resetEpoch reached; starting over...");
                this.initialize();
                tssErr = 0.0;
                rmsErr = 0.0;
                this.epoch = 1;
                totalCorrect = 0;
                totalPCorrect = new Dictionary<string,double[]>();
                continue;
            }
            this.epoch ++;
        }
        if(totalCount > 0)
        {
            this.reportFinal(this.epoch-1, tssErr, totalCorrect, totalCount, rmsErr, totalPCorrect);
            if ((this.autoCrossValidation)||(this.crossValidationCorpus.Count>0))
            {
                List<object> retval = this.sweepCrossValidation();
                double tssCVErr = (double) retval[0];
                int totalCVCorrect = (int) retval[1];
                int totalCVCount = (int) retval[2];
                double rmsCVErr = Math.Sqrt(tssCVErr/totalCVCount);
                Console.WriteLine("CV #{0,-9} | TSS Error: {1:F2} | Correct: {2:F4} | RMS Error: {3:F4}",
                          this.epoch-1, tssCVErr, (double) totalCVCorrect/totalCVCount, rmsCVErr);
                if((this.autoSaveWeightsFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                {
                    this.lastLowestTSSError = tssCVErr;
                    this.saveWeightsToFile(this.autoSaveWeightsFile);
                    Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveWeightsFile);
                }
                if((this.autoSaveNetworkFile.Length>0)&&(tssCVErr < this.lastLowestTSSError))
                {
                    this.lastLowestTSSError = tssCVErr;
                    this.saveNetworkToFile(this.autoSaveNetworkFile);
                    Console.WriteLine("auto saving weights to '{0}'...", this.autoSaveNetworkFile);
                }
            }
        }
        else
        {
            Console.WriteLine("Final: nothing done.");
        }
    }



    public void reportStart()
    {}






    public void randomizeOrder()
    {
        int[] flag = new int[this.inputs.Length];
        flag.Initialize();
        this.loadOrder = new int[this.inputs.Length];
        int pos;
        for(int i=0; i<this.inputs.Length; i++)
        {
            pos = Network.random.Next(0,this.inputs.Length);
            while(flag[pos]==1)
            {
                pos = Network.random.Next(0,this.inputs.Length);
            }
            flag[pos]=1;
            this.loadOrder[pos]=i;
        }
    }





    public List<object> sweep()
    {
        if(this.loadOrder.Length == 0)
        {
            throw new Exception("No loadOrder for inputs. Make sure inputs are properly set.");
        }
        if(this.targets.Length != this.inputs.Length)
        {
            throw new Exception("Number of inputs does not equal number of targets.");
        }
        if(this.verbosity >= 1)
        {
            Console.WriteLine("Epoch # {0}, Cycle...", this.epoch);
        }
        if(!this.orderedInputs)
        {
            this.randomizeOrder();
        }
        double tssError = 0.0;
        int totalCorrect = 0;
        int totalCount =0;
        Dictionary<string,double[]> totalPCorrect = new Dictionary<string,double[]>();
        int cnt = 0;
        foreach(int i in this.loadOrder)
        {
            //Console.WriteLine("   Processing pattern {0}", i);
            if ((this.verbosity >= 1)||(this.interactive))
            {
                Console.WriteLine("\n-----------------------------------------------------------Pattern #{0}", this.loadOrder[i]+1);
            }
            Dictionary<string,double[]> datum = this.getData(i);
            if (cnt < this.loadOrder.Length -1)
            {
                this.currentSweepCount = cnt;
            }
            else
            {
                this.currentSweepCount = 0;
            }
            this.sweeping = true;
            List<object> list = this.step(datum);
            double error = (double) list[0];
            int correct = (int) list[1];
            int total = (int) list[2];
            Dictionary<string,double[]> pcorrect = (Dictionary<string,double[]>) list[3];
            //Console.WriteLine("      Error: {1}, Correct: {1}, Total: {2}", error, correct, total);
            this.sweeping = false;
            //save Results
            tssError += error;
            totalCorrect += correct;
            totalCount += total;
            foreach(string key in pcorrect.Keys)
            {
                if(totalPCorrect.ContainsKey(key))
                {
                    totalPCorrect[key]= pcorrect[key];
                }
                else
                {
                    totalPCorrect.Add(key, pcorrect[key]);
                }
            }
            if ((cnt+1)%this.sweepReportRate==0)
            {
                Console.WriteLine("   Step # {0,6} | TSS Error: {1:F4} | Correct: {2:F4}",
                          cnt+1, tssError, (double) totalCorrect/totalCount);
            }
            //Crossvalidation check
            cnt +=1;
        }
        if (this.learning && this.batch)
        {
            this.change_weights();
        }
        List<object> ret = new List<object>();
        ret.Add(tssError);
        ret.Add(totalCorrect);
        ret.Add(totalCount);
        ret.Add(totalPCorrect);
        return ret;
    }


    public Dictionary<string,double[]> getData(int pos)
    {
        string name;
        int offset;
        double[] temp;
        Dictionary<string,double[]> retval = new Dictionary<string,double[]>();
        if (pos >= this.inputs.Length)
        {
            throw new Exception("getData() pattern beyond range.");
        }
        if (this.verbosity >=1)
        {
            Console.WriteLine("Getting input {0}...", pos);
        }
        if (this.inputMap.Count == 0)
        {
            retval.Add(this.layers[0].name, this.inputs[pos]);
        }
        else
        {
            foreach (List<object> vals in this.inputMap)
            {
                name = (string) vals[0];
                offset = (int) vals[1];
                temp = this.getDataMap("Input",pos,name,offset);
                retval.Add(name, temp);
            }

        }
        if (this.verbosity > 1)
        {
            Console.WriteLine("Loading target {0}...", pos);
        }
        if (this.targets.Length == 0)
        {}
        else
        {
            if(this.targetMap.Count ==0)
            {
                retval.Add(this.layers[this.layers.Count - 1].name, this.targets[pos]);
            }
            else
            {
                foreach (List<object> vals in this.targetMap)
                {
                    name = (string) vals[0];
                    offset = (int) vals[1];
                    retval.Add(name, this.getDataMap("target",pos,name,offset));
                }
            }
        }
        return retval;
    }


    public double[] getDataMap(string intype, int pos, string name, int offset)
    {
        double[][] vector;
        if(intype == "Input")
        {
            vector = this.inputs;
        }
        else
        {
            if(intype == "target")
            {
                vector = this.targets;
            }
            else
            {
                throw new Exception("invalid map type");
            }
        }
        double[] vec = new double[this.layersByName[name].size];
        for(int i=offset; i<offset+this.layersByName[name].size; i++)
        {
            vec[i-offset]=vector[pos][i];
        }
        return vec;
    }


    public virtual List<object> step(Dictionary<string,double[]> args)
    {
        Layer inLayer;
        Layer outLayer;
        Dictionary<string,double[]> retval = this.prepropagate(args);
        if(retval.Count>0)
        {
            args=retval;
        }
        this.propagate(args);
        Dictionary<string,double[]> retval2 = this.postpropagate(args);
        if(retval2.Count>0)
        {
            args=retval2;
        }
        foreach(string[] aa in this.association)
        {
            string inname = aa[0];
            string outname = aa[1];
            inLayer = this.getLayer(inname);
            if ((inLayer.type!= "Input") && (inLayer.type!= "Hidden"))
            {
                throw new Exception("Associated input layer not type \"Input\" or \"Hidden\"");
            }
            outLayer = this.getLayer(outname);
            if(outLayer.type != "Output")
            {
                throw new Exception("Associated output layer not type \"Output\"");
            }
            outLayer.copyTargets(inLayer.activation);
        }
        Dictionary<string,double[]> retval3 = this.prebackprop(args);
        if(retval3.Count>0)
        {
            args=retval3;
        }
        List<object> list = backprop(args);
        if(this.verbosity > 2)
        {
            this.display();
        }
        Dictionary<string,double[]> retval4 = this.postbackprop(args);
        if(retval4.Count>0)
        {
            args=retval4;
        }
        if(this.learning && !this.batch)
        {
            this.change_weights();
        }
        //this.reportPattern();
        return list;
    }

    public virtual Dictionary<string,double[]> prepropagate(Dictionary<string,double[]> args)
    {
        return args;
    }

    public virtual Dictionary<string,double[]> postpropagate(Dictionary<string,double[]> args)
    {
        return args;
    }

    public virtual Dictionary<string,double[]> prebackprop(Dictionary<string,double[]> args)
    {
        return args;
    }
    
    public virtual Dictionary<string,double[]> postbackprop(Dictionary<string,double[]> args)
    {
        return args;
    }


    public List<object> backprop(Dictionary<string,double[]> args)
    {
        List<object> retval = this.compute_error(args);
        if (this.learning)
        {
            this.compute_wed();
        }
        return retval;
    }

    public void display()
    {
        foreach(Layer layer in this.layers)
        {
            layer.display();
        }
    }


    public void compute_wed()
    {
        List<Connection> changeConnections;
        List<Layer> changeLayers;
        if(this.cacheConnections.Count != 0 )
        {
            changeConnections = this.cacheConnections;
        }
        else
        {
            changeConnections = this.connections;
        }
        changeConnections.Reverse();
        foreach (Connection connect in changeConnections)
        {
            if(connect.active&&connect.fromLayer.active && connect.toLayer.active)
            {

                for(int i=0; i<connect.fromLayer.activation.Length;i++)
                {
                    for(int j=0; j<connect.toLayer.delta.Length; j++)
                    {
                        connect.wed[i][j] += connect.fromLayer.activation[i]*connect.toLayer.delta[j];
                    }
                }
            }
        }
        if(this.cacheLayers.Count !=0)
        {
            changeLayers = this.cacheLayers;
        }
        else
        {
            changeLayers = this.layers;
        }
        foreach(Layer layer in changeLayers)
        {
            if (layer.active)
            {
                for(int i=0;i<layer.size;i++)
                {
                    layer.wed[i] += layer.delta[i];
                }
            }
        }
    }


    public void copyTargets(Layer layer, double[] vec, int start)
    {
        //vector = this.replacePatterns(vec, layer.name);
        double[] array = new double[layer.size];
        for(int i = 0; i<layer.size; i++)
        {
            array[i]=vec[i+start];
        }
        layer.copyTargets(array);
    }










    public List<object> compute_error(Dictionary<string,double[]> args)

    {
        foreach(string key in args.Keys)
        {
            Layer layer=this.getLayer(key);
            if(layer.kind=="Output")
            {
                this.copyTargets(layer,(double[])args[key],0);
            }
        }
        this.verifyTargets();
        List<object> list = this.ce_init();
        Dictionary<string,double[]> pcorrect;
        List<Connection> changeConnections ;
        if (this.cacheConnections.Count > 0)
        {
            changeConnections = this.cacheConnections;
        }
        else
        {
            changeConnections = this.connections;
        }
        double mult;
        changeConnections.Reverse();
        foreach( Connection connect in changeConnections)
        {
            if(connect.active&&connect.toLayer.active&&connect.fromLayer.active)
            {
                double[] array = new double[connect.fromLayer.size];
                array.Initialize();
                for(int i =0; i<connect.toLayer.size; i++)
                {
                    mult = this.ACTPRIME(connect.toLayer.activation[i]);
                    connect.toLayer.delta[i] = connect.toLayer.error[i]*mult;
                    for(int j=0; j<connect.fromLayer.size; j++)
                    {
                        array[j]+= connect.weight[j][i]*connect.toLayer.delta[i];
                    }
                }
                for(int i =0; i<connect.fromLayer.size; i++)
                {
                    connect.fromLayer.error[i]+= array[i];
                }
            }
        }
        pcorrect = this.getLayerErrors();
        list.Add(pcorrect);
        return list;
    }


    public double ACTPRIME(double act)
    {
        double retval = (act * (1.0 - act)) + this.sigmoid_prime_offset;
        return retval;
    }





    public List<object> ce_init()
    {
        double retval = 0.0;
        int correct = 0;
        int totalCount = 0;
        foreach(Layer layer in this.layers)
        {
            if(layer.active)
            {
                if (layer.type == "Output")
                {
                    layer.error = this.errorFunction(layer.target, layer.activation);
                    totalCount += layer.size;
                    for(int i=0;i<layer.size;i++)
                    {
                        retval += Math.Pow((layer.target[i]-layer.activation[i]),2);
                        if(Math.Abs(layer.target[i]-layer.activation[i])<this.tolerance)
                        {
                            correct += 1;
                        }
                    }
                }
                else
                {
                    if(layer.type=="Hidden")
                    {
                        for(int i=0; i<layer.size;i++)
                        {
                            layer.error[i]=0;
                        }
                    }
                }
            }
        }
        List<object> list = new List<object>();
        list.Add(retval);
        list.Add(correct);
        list.Add(totalCount);
        return list;
    }


    public double[] errorFunction(double[] tar, double[] act)
    {
        double[] array = new double[tar.Length];
        if(this.hyperbolicError)
        {
            double err;
            for(int i =0; i<tar.Length; i++)

            {
                err = tar[i]-act[i];
                if(err<-.9999999)
                {
                    array[i]=-17.0;
                }
                else
                {
                    if(err>.9999999)
                    {
                        array[i]=17.0;
                    }
                    else
                    {
                        array[i]=Math.Log((1.0+err)/(1.0-err));
                    }
                }
            }
            return array;
        }
        else
        {
            for(int i =0; i<tar.Length; i++)
            {
                array[i]=(tar[i]-act[i]);
            }
            return array;
        }
    }


    public double[] deltaWeight(double e, double[] wed, double m, double[] dweightLast, double[] wedLast, double[] w, int n)
    {
        double[] nextStep = new double[dweightLast.Length];
        nextStep.Initialize();
        double shrinkFactor = this.mu/(1+this.mu);
        if (this.splitEpsilon!=0)
        {
            e = e/n;
        }
        if (this.quickprop)
        {
            double s;
            double d;
            double p;
            for(int i=0; i<dweightLast.Length; i++)
            {
                s = wed[i];
                d = dweightLast[i];
                p = wedLast[i];
                if (d> 0.0)
                {
                    if (s > 0.0)
                    {
                        nextStep[i] += (e * s);
                    }
                    if (s >= (shrinkFactor * p))
                    {
                        nextStep[i] += (this.mu*d);
                    }
                    else
                    {
                        nextStep[i] += (d*s/(p-s));
                    }
                }
                else
                {
                    if (d < 0.0)
                    {
                        if (s< 0.0)
                        {
                            nextStep[i] += (e*s);
                        }
                        if (s<= (shrinkFactor*p))
                        {
                            nextStep[i]+= (this.mu*d);
                        }
                        else
                        {
                            nextStep[i] = nextStep[i]+(d*s/(p-s));
                        }
                    }
                    else
                    {
                        nextStep[i]+= (e*s+m*d);
                    }
                }
            }
        }
        else
        {
            for(int i = 0; i < dweightLast.Length; i++)
            {
                nextStep[i]= e*wed[i] + m*dweightLast[i];
            }
        }
        return nextStep;
    }



    public List<double> change_weights()
    {
        int dw_count = 0;
        double dw_sum = 0.0;
        List<Layer> changeLayers;
        if (this.cacheLayers.Count > 0)
        {
            changeLayers = this.cacheLayers;
        }
        else
        {
            changeLayers = this.layers;
        }
        foreach (Layer layer in changeLayers)
        {
            if(layer.active&&(layer.type!="Input")&&!layer.frozen)
            {
                if ((this.quickprop)||(this.splitEpsilon==1))
                {
                    layer.dweight = this.deltaWeight(this.epsilon, layer.wed, this.momentum, layer.dweight,
                                     layer.wedLast, layer.weight, this.numConnects(layer.name));
                    for (int i=0; i< layer.size; i++)
                    {
                        layer.weight[i] += layer.dweight[i];
                    }

                }
                else
                {
                    for(int i=0; i<layer.size; i++)
                    {
                        layer.dweight[i] = this.epsilon*layer.wed[i]+this.momentum*layer.dweight[i];
                        layer.weight[i] += layer.dweight[i];
                    }
                }

                layer.wedLast = Functions.copyDoubleArray(layer.wed);
                if(this.quickprop)
                {
                    for(int i=0; i<layer.size;i++)
                    {
                        layer.wed[i]=layer.weight[i]*this.decay;
                    }
                }
                else
                {
                    for(int i=0;i<layer.size;i++)
                    {
                        layer.wed[i]=0;
                    }
                }
                dw_count += layer.dweight.Length;
                foreach(double d in layer.dweight)
                {
                    dw_sum += Math.Abs(d);
                }
            }
        }
        List<Connection> changeConnections;
        if (this.cacheConnections.Count >0)
        {
            changeConnections = this.cacheConnections;
        }
        else
        {
            changeConnections = this.connections;
        }
        foreach(Connection connect in changeConnections)
        {
            if(connect.active&&connect.fromLayer.active&&connect.toLayer.active&&!connect.frozen)
            {
                if ((this.quickprop) ||(this.splitEpsilon==1))
                {
                    for (int i = 0; i<connect.fromLayer.size; i++)
                    {
                        connect.dweight[i] = this.deltaWeight(this.epsilon, connect.wed[i], this.momentum,
                                              connect.dweight[i], connect.wedLast[i], connect.weight[i],
                                              this.numConnects(connect.toLayer.name));


                        for(int j = 0; j<connect.toLayer.size; j++)
                        {
                            connect.weight[i][j]+=connect.dweight[i][j];
                        }
                    }
                }
                else
                {

                    for(int i=0; i<connect.fromLayer.size; i++)
                    {
                        for(int j=0;j<connect.toLayer.size;j++)
                        {
                            connect.dweight[i][j]=this.epsilon*connect.wed[i][j]+this.momentum*connect.dweight[i][j];
                            connect.weight[i][j]+= connect.dweight[i][j];
                        }
                    }

                }
                if(this.quickprop)
                {
                    connect.wedLast = Functions.copyDoubleMatrix(connect.wed);
                    for(int i=0;i<connect.fromLayer.size; i++)
                    {
                        for(int j=0; j<connect.toLayer.size;j++)
                        {
                            connect.wed[i][j] = connect.weight[i][j]*this.decay;
                        }
                    }
                }
                else
                {
                    for(int i=0;i<connect.fromLayer.size;i++)
                    {
                        for(int j=0; j<connect.toLayer.size;j++)
                        {
                            connect.wed[i][j]=0;
                        }
                    }
                }
                dw_count += connect.dweight.Length*connect.dweight[0].Length;
                foreach(double[] l in connect.dweight)
                {
                    foreach(double i in l)
                    {
                        dw_sum+=Math.Abs(i);
                    }
                }
            }
        }
        if (this.verbosity >4)
        {
            Console.WriteLine("\n #### Weights changed. ####");
            foreach(Connection con in this.connections)
            {
                con.display();
            }
            Console.WriteLine("");
        }
        else
        {
            if(verbosity>0)
            {
                Console.WriteLine("Weights changed.");
                foreach(Connection con in this.connections)
                {
                    con.display();
                } 
            }
        }
        List<double> list = new List<double>();
        list.Add(dw_count);
        list.Add(dw_sum);
        return list;
    }

    public int numConnects(string layerName)
    {
        int count = 0;
        if(this.layersByName[layerName].active)
        {
            count += 1;
            foreach(Connection con in this.connections)
            {
                if(con.active&&con.fromLayer.active&&(con.toLayer.name==layerName))
                {
                    count+= con.fromLayer.size;
                }
            }
        }
        return count;
    }


    public Dictionary<string, double[]> getLayerErrors()
    {
        Dictionary<string, double[]> pcorrect = new Dictionary<string, double[]>();
        double[] lyst = new double[4];
        foreach (Layer layer in this.layers)
        {
            if (layer.patternReport)
            {
                lyst[0]=0;
                lyst[1]=0;
                lyst[2]=0;
                lyst[3]=0;
                for(int i =0; i<layer.size; i++)
                {
                    double b = Math.Abs(layer.target[i]-layer.activation[i]);
                    if(b<this.tolerance)
                    {
                        lyst[0]+=b;
                    }
                }
                lyst[2]+=this.compare(layer.target, layer.activation);

                lyst[1] = layer.size;
                lyst[3]+=1;
                pcorrect.Add(layer.name,lyst);
            }
        }
        return pcorrect;
    }


    public Dictionary<string,double[]> propagate(Dictionary<string,double[]> args)
    {
        Layer lay;
        foreach(string key in args.Keys)
        {
            lay = this.getLayer(key);
            if (lay.kind=="Input")
            {
                this.copyActivations(lay, args[key]);
            }
            else
            {
                if(lay.kind=="Context")
                {
                    this.copyActivations(lay, args[key]);
                }
                else
                {
                    if((lay.kind=="Output")&&(args[key].Length==lay.size))
                    {
                        this.copyTargets(lay, args[key]);
                    }
                }
            }
        }
        //this.verifyInputs();
        if (this.verbosity > 2)
        {
            Console.WriteLine("Propagate Network '{0}':", this.name);
        }
        foreach(Layer layer in this.layers)
        {
            if    ((layer.type != "Input" ) && layer.active)
            {
                layer.netinput = Functions.copyDoubleArray(layer.weight);
            }
        }
        double temp;
        foreach(Layer layer in this.layers)
        {
            if(layer.active)
            {
                foreach(Connection connect in this.connections)
                {
                    if((connect.toLayer.name==layer.name)&&(connect.fromLayer.active)&&(connect.active))
                    {
                        temp=0.0;

                        for(int j=0;j<connect.toLayer.size;j++)
                        {
                            for(int i=0; i<connect.fromLayer.size;i++)
                            {
                                temp += connect.fromLayer.activation[i]*connect.weight[i][j];
                            }
                            connect.toLayer.netinput[j]+= temp;
                        }
                    }
                }
                if (layer.type!= "Input")
                {
                    layer.activation = this.act.activationFunction(layer.netinput);
                }
            }
        }
        this.count +=1;
        Dictionary<string,double[]> dict = new Dictionary<string,double[]>();
        if (args.Count !=0)
        {
            foreach(Layer layer in this.layers)
            {
                if(layer.type=="Output")
                {
                    dict.Add(layer.name, Functions.copyDoubleArray(layer.activation));
                }
            }
        }
        return dict;
    }

    public double[] activationFunction(double[] array)
    {
        double[] array2 = new double[array.Length];
        double temp;
        for(int i =0; i<array.Length; i++)
        {
            temp = array[i];
            if (temp<-15.0)
            {
                array2[i]=0.0;
            }
            else
            {
                if (temp>15.0)
                {
                    array2[i]= 1.0;
                }
                else
                {
                    array2[i] = (1.0/(1.0+Math.Exp(-temp)));
                }
            }
        }
        return array2;
    }





    public void copyTargets(Layer layer, double[] vector)
    {
        if(this.verbosity>4)
        {
            Console.Write("Copying Target: ");
            foreach(double no in vector)
            {
                Console.Write("{0} ", no);
            }
            Console.Write("\n");
        }
        layer.copyTargets(vector);
    }



    public void copyActivations(Layer layer, double[] vector)
    {
        if(this.verbosity>4)
        {
            Console.Write("Copying Activations: ");
            foreach(double no in vector)
            {
                Console.Write("{0} ", no);
            }
            Console.Write("\n");
        }
        layer.copyActivations(vector);
    }


    public void addLayers(params int[] arg)
    {
        //string netType="serial";
        this.addLayer("Input", arg[0]);
        List<string> hiddens = new List<string>();
        string name;
        if(arg.Length>3)
        {
            for(int i=1;i<arg.Length-1;i++)
            {
                name = "Hidden"+(i-1).ToString();
                this.addLayer(name, arg[i]);
                hiddens.Add(name);
            }
        }
        else
        {
            if(arg.Length==3)
            {
                name="Hidden";
                this.addLayer(name, arg[1]);
                hiddens.Add(name);
            }
            else
            {
                if(arg.Length==2)
                {}
                else
                {
                    throw new Exception("Not enough layers! Need at least 2!");
                }
            }
        }
        this.addLayer("Output", arg[arg.Length-1]);
        string lastName = "Input";
        foreach(string _name in hiddens)
        {
            this.connect(lastName, _name);
            lastName = _name;
        }
        this.connect(lastName, "Output");
    }

    public void setHyperbolicError(bool value)
    {
        this.hyperbolicError = value;
    }


    public void saveNetworkToFile(string filename)
    {
        TextWriter tw = new StreamWriter(filename);
        tw.WriteLine("network,network");
        foreach(Layer layer in this.layers)
        {
            tw.WriteLine("layer,{0},{1}", layer.name, layer.size);
            for(int i =0; i<layer.size; i++)
            {
                tw.Write("{0} ", layer.weight[i]);
            }
            tw.Write("\n");
        }
        foreach(Connection con in this.connections)
        {
            tw.WriteLine("connection,{0},{1}", con.fromLayer.name, con.toLayer.name);
            for(int i =0; i<con.fromLayer.size;i++)
            {
                for(int j =0; j<con.toLayer.size; j++)
                {
                    tw.Write("{0} ", con.weight[i][j]);
                }
                tw.Write("\n");
            }
        }
        tw.Write("0");
        tw.Close();
    }


    public static Network loadNetworkFromFile(string filename)
    {
        Network network;
        TextReader tr = new StreamReader(filename);
        string line = tr.ReadLine();
        line.Replace("\n","");
        string[] list = line.Split(',');
        string type = list[1];
        if(type=="network")
        {
            network = new Network();
        }
        else
        {
            throw new Exception("Unknown network type.");
        }
        int connectCount = -1;
        line = tr.ReadLine();
        line.Replace("\n","");
        while(line.Length >  4)
        {
            line.Replace("\n","");
            if(line.StartsWith("layer,"))
            {
                string[] lyst = line.Split(',');
                string name = lyst[1];
                int size = int.Parse(lyst[2]);
                network.addLayer(name,size);
                line = tr.ReadLine();
                line.Replace(" \n","");
                string[] weights = line.Split(' ');
                for(int i =0; i<network.layersByName[name].size; i++)
                {
                    network.layersByName[name].weight[i] = double.Parse(weights[i]);
                }
            }
            else
            {
                if(line.StartsWith("connection,"))
                {
                    Console.WriteLine("check 1");
                    string[] lyst = line.Split(',');
                    string fromName = lyst[1];
                    string toName = lyst[2];
                    Console.WriteLine("check 2");
                    network.connect(fromName, toName);
                    Console.WriteLine("check 3");
                    connectCount += 1;
                    for(int i=0; i<network.layersByName[fromName].size; i++)
                    {
                        line = tr.ReadLine();
                        line.Replace(" \n","");
                        string[] weights = line.Split(' ');
                        Console.WriteLine("Connection from {0} to {1}, length {2}",
                                  fromName, toName, network.layersByName[toName].size);
                        for(int j=0; j<network.layersByName[toName].size;j++)
                        {
                            Console.WriteLine("j = {0}", j);
                            network.connections[connectCount].weight[i][j]= double.Parse(weights[j]);
                        }
                    }
                }
            }
            line = tr.ReadLine();
        }
        return network;
    }



    public List<object> sweepCrossValidation()
    {
        bool oldLearning = this.learning;
        this.learning = false;
        double tssError = 0;
        int totalCorrect=0;
        int totalCount = 0;
        Dictionary<string, double[]> totalPCorrect = new Dictionary<string, double[]>();
        this.cv = true;
        if(this.autoCrossValidation)
        {
            for(int i =0; i < this.inputs.Length; i++)
            {
                Dictionary<string,double[]> set
                    = this.getDataCrossValidation(i);
                this.sweeping = true;
                List<object> lyst = this.step(set);
                this.sweeping = false;
                tssError += (double) lyst[0];
                totalCorrect += (int) lyst[1];
                totalCount += (int) lyst[2];
                totalPCorrect = Functions.sumMerge(totalPCorrect, (Dictionary<string,double[]>) lyst[3]);
            }
        }
        else
        {
            foreach(Dictionary<string,double[]> set
                    in this.crossValidationCorpus)
            {
                this.sweeping = true;
                List<object> lyst = this.step(set);
                this.sweeping = false;
                if (this.crossValidationReportLayers.Count > 0)
                {
                    lyst = this.getError(this.crossValidationReportLayers);
                }
                tssError += (double) lyst[0];
                totalCorrect += (int) lyst[1];
                totalCount += (int) lyst[2];
                totalPCorrect = Functions.sumMerge(totalPCorrect, (Dictionary<string,double[]>) lyst[3]);
            }
        }
        this.learning = oldLearning;
        this.cv = false;
        List<object> ret = new List<object>();
        ret.Add(tssError);
        ret.Add(totalCorrect);
        ret.Add(totalCount);
        ret.Add(totalPCorrect);
        return ret;
    }


    public List<object> getError(List<string> layerNames)
    {
        double totalSquaredError = 0;
        int totalSize=0;
        int totalCorrect = 0;
        Dictionary<string,double[]> pcorrect = new Dictionary<string,double[]>();
        foreach(string layerName in layerNames)
        {
            double[] lyst = new double[]{0,0,0,0};
            Layer layer = this.layersByName[layerName];
            totalSize+= layer.size;
            for(int i=0;i<layer.size;i++)
            {
                double temp = layer.target[i]-layer.activation[i];
                totalSquaredError += Math.Pow(temp,2);
                if(Math.Abs(temp)<this.tolerance)
                {
                    totalCorrect += 1;
                    lyst[0]+=1;
                }
            }
            if (layer.patternReport)
            {
                lyst[1]=layer.size;
                if (this.compare(layer.target, layer.activation)==1)
                {
                    lyst[2]+=1;
                }
                lyst[3]+=1;
                pcorrect.Add(layerName, lyst);
            }
        }
        List<object> ret = new List<object>();
        ret.Add(totalSquaredError);
        ret.Add(totalCorrect);
        ret.Add(totalSize);
        ret.Add(pcorrect);
        return ret;
    }




    public int compare(double[] v1, double[] v2)
    {
        if (v1.Length != v2.Length)
        {
            return 0;
        }
        for(int i =0; i<v1.Length; i++)
        {
            if (Math.Abs(v1[i]-v2[i])>this.tolerance)
            {
                return 0;
            }
        }
        return 1;
    }


    public Dictionary<string, double[]> getDataCrossValidation(int pos)
    {
        Dictionary<string, double[]> set
            = new Dictionary<string, double[]>();
        set.Add("Input", this.inputs[pos]);
        if (this.targets.Length >0)
        {
            set.Add("Output", this.targets[pos]);
        }
        return set
                   ;
    }

    public void setAutoCrossValidation(bool value)
    {
        this.autoCrossValidation = value;
    }

    public void setAutoSaveNetworkFile(string filename)
    {
        this.autoSaveNetworkFile=filename;
        this.autoSaveNetworkFileFormat = "conx";
    }


    public void saveWeightsToFile(string filename)
    {
        Console.WriteLine("Attention: saveWeightsToFile() not defined yet!");
        Console.WriteLine("Saving network to file instead...");
        this.saveNetworkToFile(filename);
    }

    public void loadWeightsFromFile(string filename)
    {
        Console.WriteLine("loadWeightsFromFile() not defined yet.");
        Console.WriteLine("Try loadNetworkFromFile() instead!");
    }


    public void loadInputsFromFile(string filename)
    {
        this.inputs = this.loadVectorsFromFile(filename);
        this.loadOrder = new int[this.inputs.Length];
        for(int i=0;i<this.inputs.Length;i++)
        {
            this.loadOrder[i]=i;
        }
    }

    public void saveInputsToFile(string filename)
    {
        TextWriter tw = new StreamWriter(filename);
        foreach(double[] input in this.inputs)
        {
            foreach(double value in input)
            {
                tw.Write("{0} ", value);
            }
            tw.Write("\n");
        }
        tw.Write("\n");
        tw.Close();
    }

    public void loadTargetsFromFile(string filename)
    {
        this.targets = this.loadVectorsFromFile(filename);
    }


    public void saveTargetsToFile(string filename)
    {
        TextWriter tw = new StreamWriter(filename);
        foreach(double[] target in this.targets)
        {
            foreach(double value in target)
            {
                tw.Write("{0} ", value);
            }
            tw.Write("\n");
        }
        tw.Write("\n");
        tw.Close();
    }

    public double[][] loadVectorsFromFile(string filename)
    {
        TextReader tr = new StreamReader(filename);
        string line = tr.ReadLine();
        int lineno = 0;
        int lastLength =-1;
        ArrayList data = new ArrayList();
        while(line.Length>0)
        {
            line.Replace("\n","");
            string[] linedata = line.Split(' ');
            ArrayList newdata = new ArrayList();
            foreach(string str in linedata)
            {
                try
                {
                    newdata.Add(double.Parse(str));
                }
                catch {}
            }
            if((lastLength==-1)||(newdata.Count==lastLength))
            {
                data.Add((double[]) newdata.ToArray(typeof(double)));
            }
            else
            {
                Console.WriteLine("Data Format Error: line = {0}", lineno);
                throw new Exception("DataFormatError");
            }
            lastLength = newdata.Count;
            lineno++;
            line = tr.ReadLine();
        }
        double[][] adata = (double[][]) data.ToArray(typeof(double[]));
        return adata;
    }

    public void saveDataToFile(string filename)
    {
        TextWriter tw = new StreamWriter(filename);
        for(int i=0; i<inputs.Length; i++)
        {
            foreach(double item in this.inputs[i])
            {
                tw.Write("{0} ", item);
            }
            foreach(double item in this.targets[i])
            {
                tw.Write("{0} ", item);
            }
            tw.Write("\n");
        }
        tw.Write("\n");
        tw.Close();
    }

    public void loadDataFromFile(string filename)
    {
        int insize = this.layersByName["Input"].size;
        int outsize = this.layersByName["Output"].size;
        TextReader tr = new StreamReader(filename);
        string line = tr.ReadLine();
        ArrayList targetsList = new ArrayList();
        ArrayList inputsList = new ArrayList();
        while(line.Length>2)
        {
            string[] linedata = line.Split(' ');
            if(linedata.Length!=(insize+outsize+1))
            {
                Console.WriteLine("Size mismatch.");
                Console.WriteLine("Input Layer size: {0}, Output Layer size: {1}, Data: {2}",
                          insize, outsize, linedata.Length-1);
                Console.Write("Data in line: ");
                foreach(string item in linedata)
                {
                    Console.Write("'{0}' ", item);
                }


                throw new Exception();
            }
            double[] inarray = new double[insize];
            double[] outarray = new double[outsize];
            Console.WriteLine("In size: {0}, out size: {1}", insize, outsize);
            for(int i=0; i<insize;i++)
            {
                Console.Write(i);
                inarray[i]=double.Parse(linedata[i]);
            }
            Console.Write(" ");
            for(int j=insize;j<insize+outsize;j++)
            {
                outarray[j-insize]=double.Parse(linedata[j]);
            }
            inputsList.Add(inarray);
            targetsList.Add(outarray);
            line = tr.ReadLine();
        }
        this.inputs = (double[][]) inputsList.ToArray(typeof(double[]));
        this.targets = (double[][]) targetsList.ToArray(typeof(double[]));
        this.loadOrder = new int[insize];
        for (int i=0; i<insize;i++)
        {
            this.loadOrder[i]=i;
        }
    }


    public void useASIGActivationFunction()
    {
        this.act = new ActASIG(this.sigmoid_prime_offset);
        foreach(Layer layer in this.layers)
        {
            layer.minActivation = this.act.minActivation;
            layer.maxActivation = this.act.maxActivation;
            layer.minTarget = this.act.minTarget;
            layer.maxTarget = this.act.maxTarget;
        }
    }
    
    public void useFahlmanActivationFunction()
    {
        this.act = new ActFahlman(this.sigmoid_prime_offset);
        foreach(Layer layer in this.layers)
        {
            layer.minActivation = this.act.minActivation;
            layer.maxActivation = this.act.maxActivation;
            layer.minTarget = this.act.minTarget;
            layer.maxTarget = this.act.maxTarget;
        }
    }


    public void useTANHActivationFunction()
    {
        this.act = new ActTANH(this.sigmoid_prime_offset);
        foreach(Layer layer in this.layers)
        {
            layer.minActivation = this.act.minActivation;
            layer.maxActivation = this.act.maxActivation;
            layer.minTarget = this.act.minTarget;
            layer.maxTarget = this.act.maxTarget;
        }
    }

    public void setSigmoidOffset(double value)
    {
        this.sigmoid_prime_offset = value;
        this.act.sigmoid_prime_offset = value;
    }
    

    public void add(Layer layer)
    {
        layer.maxRandom = this.maxRandom;
        layer.minTarget = this.act.minTarget;
        layer.minActivation = this.act.minActivation;
        layer.maxTarget = this.act.maxTarget;
        layer.maxActivation = this.act.maxActivation;
        this.layers.Add(layer);
        this.layersByName.Add(layer.name, layer);
    }


    static void Main()
    {

        


        
        double[][] inputs = new double[10][];
        double[][] targets = new double[10][];
        int temp;
        for(int i=0;i<10;i++)
        {   
            double[] array = new double[25000];
            temp = Network.random.Next(0,2);
            int a = 0;
            int b = 0;
            int c = 0;
            int d = 0;
            int e = 0;
                       
            for(int j=0;j<25000;j++)
            {
                temp = Network.random.Next(0,2);
                a = (a+temp)%2;
                b = (b+temp)%3;
                c = (c+temp)%5;
                d = (d+temp)%7;
                e = (e+temp)% 11;
                array[j] = temp;
            }
            
            double[] tar = new double[]{a, b/2.0, c/4.0, d/6.0, e/10.0};
            inputs[i]=Functions.copyDoubleArray(array);
            targets[i]=Functions.copyDoubleArray(tar);
        }
         Network net = new Network();
         net.addLayers(25000,1000,5);
         net.setVerbosity(0);
         net.setInputs(inputs);
         net.setTargets(targets);
         net.setReportRate(1);
         net.setResetEpoch(100000);
         net.setTolerance(.4);
         Console.WriteLine("Generated patterns");
         DateTime start = DateTime.Now;
         net.train(0,10);
         TimeSpan timeTaken = DateTime.Now - start;
         Console.WriteLine("Minutes: " + timeTaken.Minutes);
         Console.WriteLine("Seconds: " + timeTaken.Seconds);
         Console.WriteLine("Milliseconds: " + timeTaken.Milliseconds);
         
        
        
        
        /*
        Random rand = new Random();
        SRN net = new SRN();
        net.addSRNLayers(3,3,3);
        net.tolerance = .3;
        double[][] inputs = new double[99][];
        double temp = 0;
        for(int i = 0; i< 99; i++)
        {
            if(i%3 == 0)
            {
                inputs[i] = new double[]{.9,.1,.1};
            }
            else
            {
                if (temp==2)
                {
                    inputs[i] = new double[]{.1,.9,.1};
                    temp = 0;
                }
                else
                {
                    if (temp == 3)
                    {
                        inputs[i] = new double[]{.1,.1,.9};
                        temp = 0;
                    }
                    else
                    {
                        temp = rand.Next(2,4);
                        if (temp==2)
                        {
                            inputs[i] = new double[]{.1,.1,.9};
                        }
                        else
                        {
                            inputs[i] = new double[]{.1,.9,.1};
                        }
                    }
                }
            }
        }
        double[][] targets = new double[99][];
        for(int i=0; i<98;i++)
        {
            targets[i] = inputs[i+1];
        }
        targets[98] = new double[]{.9,.1,.1};
                        
        net.setInputs(inputs);
        net.setTargets(targets);
        net.setSequenceType("ordered-continuous");
        net.setReportRate(10);
        net.train();


        */


/*

        //very simple Network for debugging


        double[][] inputs = new double[2][];
        double[][] targets = new double[2][];
        double[] a1 = new double[]{0,1};
        double[] a2 = new double[]{1,0};
        double[] b1 = new double[]{1};
        double[] b2 = new double[]{0};
        inputs[0]=a1;
        inputs[1]=a2;
        targets[0]=b1;
        targets[1]=b2;

        Network net = new Network();
        net.addThreeLayers(2,1,1);
        net.setInputs(inputs);
        net.setTargets(targets);
        net.setTolerance(.4);
        net.setVerbosity(0);
        net.setReportRate(1);
        net.saveInputsToFile("intest.conx");
        net.loadInputsFromFile("intest.conx");
        net.saveTargetsToFile("tartest.conx");
        net.loadTargetsFromFile("tartest.conx");
        net.layers[0].weight[0]=0;
        net.layers[0].weight[1]=0;
        net.layers[1].weight[0]=0;
        net.layers[2].weight[0]=0;
        net.connections[0].weight[0][0]=0;
        net.connections[0].weight[1][0]=0;
        net.connections[1].weight[0][0]=0;
        net.setOrderedInputs(true);
        net.setVerbosity(0);
        net.train(0,1000);


        Console.WriteLine("Done.");
*/
    }
}








public class SRN: Network
{
    bool initContext = false;
    bool learnDuringSequencing = true;
    bool contextCopying = true;
    Dictionary<string, Layer> contextLayers = new Dictionary<string, Layer>();
    List<string[]> prediction;
    string sequenceType="";

    public SRN(): base()
    {
        this.name = "Simple Recurrent Network";
        Console.WriteLine("Remember to set sequence type.");
    }
    
    public void setSequenceType(string value)
    {
        if (value == "ordered-continuous")
        {
            this.orderedInputs = true;
            this.initContext = false;
            this.sequenceType = value;
        }
        if (value == "random-segmented")
        {
            this.orderedInputs = false;
            this.initContext = true;
            this.sequenceType = value;
        }
        if (value == "random-continuous")
        {
            this.orderedInputs = false;
            this.initContext = false;
            this.sequenceType = value;
        }
        if (value == "ordered-segmented")
        {
            this.orderedInputs = true;
            this.initContext = true;
            this.sequenceType = value;
        }
        if (this.sequenceType.Length<1)
        {
            Console.WriteLine("Please set valid sequence type.");
            Console.WriteLine("Options: \"ordered-continuous\", \"random-segmented\", \"random-continuous\", \"ordered-segmented\".");
            throw new Exception("Sequence type not set.");
        }
    }
    
    
    public void predict(string inName, string outName)
    {
        string[] array = new string[]{inName, outName};
        this.prediction.Add(array);
    }
    
    
    public void setInitContext(bool value)
    {
        this.initContext = value;
    }
    
            
    public void setLearningDuringSequence(bool value)
    {
        this.learnDuringSequencing = value;
    }
    
    
    public override void addThreeLayers(int inc, int hidc, int outc)
    {
        this.addLayer("Input", inc);
        this.addContextLayer("Context", hidc, "Hidden");
        this.addLayer("Hidden", hidc);
        this.addLayer("Output", outc);
        this.connect("Input", "Hidden");
        this.connect("Context", "Hidden");
        this.connect("Hidden", "Output");
    }
    

    public void addSRNLayers(int inc, int hidc, int outc)
    {
        this.addThreeLayers(inc, hidc, outc);
    }
    
    
    public void addContextLayer(string name, int size, string hiddenLayerName)
    {
        Layer layer = new Layer(name, size);
        this.addContext(layer, hiddenLayerName);
    }
    

    public void addContextLayer(string name, int size, string hiddenLayerName, int verbosity)
    {
        Layer layer = new Layer(name, size);
        layer.verbosity = verbosity;
        this.addContext(layer, hiddenLayerName);
    }


    public void addContext(Layer layer, string hiddenLayerName)
    {
        this.add(layer);
        if (this.contextLayers.ContainsKey(hiddenLayerName))
        {
            throw new Exception("There is already a context Layer associated with this layer.");
        }
        else
        {
            this.contextLayers.Add(hiddenLayerName, layer);
            layer.kind = "Context";
        }
    }


    public void copyHiddenToContext()
    {
        foreach(string key in this.contextLayers.Keys)
        {
            if(this.verbosity>2)
            {
                Console.WriteLine("Hidden Layer: {0}", this.getLayer(key).activation);
                Console.WriteLine("Context Layer before copy: {0}", this.contextLayers[key].activation);
            }
            this.contextLayers[key].copyActivations(this.getLayer(key).activation);
            if(this.verbosity>2)
            {
                Console.WriteLine("Context Layer after copy: {0}", this.contextLayers[key].activation);
            }
        }
    }
    
        

    public void setContext(double value)
    {
        foreach( Layer context in this.contextLayers.Values)
        {
            context.resetFlags();
            context.setActivations(value);
        }
    }
    
    
    public override Dictionary<string, double[]> prepropagate(Dictionary<string, double[]> args)
    {
        if(!this.contextCopying)
        {
            foreach(Layer layer in this.layers)
            {
                if (layer.kind == "Context")
                {
                    layer.activationSet = true;
                }
            }
        }
        return args;
    }
    
    
    public override Dictionary<string, double[]> postbackprop(Dictionary<string,double[]> args)
    {
        if(this.contextCopying)
        {
            this.copyHiddenToContext();
        }
        return args;
    }


    public List<object> networkStep(Dictionary<string,double[]> args)
    {
        return base.step(args);
    }
    
    
    public override List<object> step(Dictionary<string,double[]> args)
    {
        foreach(string name in args.Keys)
        {
            Console.WriteLine(name);
        }
        if(this.sequenceType.Length<1)
        {
            throw new Exception("sequenceType not set! Use SRN.setSequenceType()");
        }
        if (this.initContext)
        {
            this.setContext(.5);
        }
        else
        {
            foreach(Layer context in this.contextLayers.Values)
            {
                context.activationSet = true;
            }
        }
        List<string> inputBankNames = new List<string>();
        List<string> outputBankNames = new List<string>();
        int inputBankSize =0;
        int inputArgSize = 0;
        foreach(Layer layer in this.layers)
        {
            if (layer.kind == "Input")
            {
                Console.WriteLine("Found input layer: {0}", layer.name);
                inputBankNames.Add(layer.name);
                inputBankSize+=layer.size;
                if (args.ContainsKey(layer.name))
                {
                    inputArgSize+= args[layer.name].Length;
                }
            }
            if (layer.kind == "Output")
            {
                Console.WriteLine("Found output layer: {0}", layer.name);
                outputBankNames.Add(layer.name);
            }
        }
        int sequenceLength = inputArgSize/inputBankSize;
        bool learning = this.learning;
        List<object> retval = new List<object>();
        double error = 0.0;
        int correct = 0;
        int count = 0;
        Dictionary<string,double[]> totalPCorrect = new Dictionary<string, double[]>();
        for(int step=0; step<sequenceLength; step++)
        {
            if ((this.verbosity >=1)||this.interactive)
            {
                Console.WriteLine("\n-----------------------------------------------------------Step #{0}", step+1);
            }
            Dictionary<string,double[]> dict = new Dictionary<string,double[]>();
            foreach(string key in args.Keys)
            {
                dict.Add(key, Functions.copyDoubleArray(args[key]));
            }
            foreach(string name in inputBankNames)
            {
                if(args.ContainsKey(name))
                {
                    Console.WriteLine("Input bank name: {0}", name);
                    int patternLength = this.layersByName[name].size;
                    int offset = step* patternLength;
                    double[] arr = new double[patternLength];
                    if ((offset+patternLength) >= args[name].Length)
                    {
                        Array.Copy(args[name],args[name].Length-patternLength,arr,0,patternLength);
                    }
                    else
                    {
                        Array.Copy(args[name],offset, arr, 0, patternLength);
                    }
                    dict[name]=arr;
                }
            }
            foreach(string name in outputBankNames)
            {
                if(args.ContainsKey(name))
                {
                    Console.WriteLine("Output Bank Name: {0}", name);
                    int patternLength = this.layersByName[name].size;
                    int offset = step* patternLength;
                    double[] arr = new double[patternLength];
                    if ((offset+patternLength) >= args[name].Length)
                    {
                        Array.Copy(args[name],args[name].Length-patternLength,arr,0,patternLength);
                    }
                    else
                    {
                        Array.Copy(args[name],offset, arr, 0, patternLength);
                    }
                    dict[name]=arr;
                }
            }
            foreach(string[] tuple in this.prediction)
            {
                string inName = tuple[0];
                string outName = tuple[1];
                Layer inLayer = this.getLayer(inName);
                if(inLayer.type!="Input")
                {
                    throw new Exception("Prediction input not type \"Input\".");
                }
                Layer outLayer = this.getLayer(outName);
                if(outLayer.type!="Output")
                {
                    throw new Exception("Prediction output not type \"Output\".");
                }
                int patternLength = inLayer.size;
                // Correct??? Issues with patternLength!
                if(step==sequenceLength-1)
                {
                    int start = 0;
                    if(!this.sweeping)
                    {
                        throw new Exception("Attemping to predict last item in sequence, but using step(). Use sweep() instead.");
                    }
                    if(this.currentSweepCount==0)
                    {
                        Dictionary<string,double[]> pattern = this.getData(this.loadOrder[0]);
                        if((inputBankNames.Contains(inName))&&pattern.ContainsKey(inName))
                        {
                            double[] arr = new double[patternLength];
                            Array.Copy(pattern[inName],start,arr,0,patternLength);
                            dict[outName]=arr;
                        }
                    }
                    else
                    {
                        Dictionary<string,double[]> pattern = this.getData(this.loadOrder[currentSweepCount+1]);
                        if((inputBankNames.Contains(inName))&&pattern.ContainsKey(inName))
                        {
                            double[] arr = new double[patternLength];
                            Array.Copy(pattern[inName],start,arr,0,patternLength);
                            dict[outName]=arr;
                        }
                    }
                }
                else
                {
                    int start = (step+1)*inLayer.size;
                    double[] arr = new double[patternLength];
                    Array.Copy(args[inName],start,arr,0,patternLength);
                    dict[outName]=arr;
                }
            }
            if (step<sequenceLength-1)
            {
                if(!this.learnDuringSequencing)
                {
                    this.learning = false;
                }
            }
            List<object> ret = this.networkStep(dict);
            this.learning = learning;
            error += (double)ret[0];
            correct += (int) ret[1];
            count += (int) ret[2];
            totalPCorrect = Functions.sumMerge(totalPCorrect, (Dictionary<string,double[]>) ret[3]);
        }
        retval.Add(error);
        retval.Add(correct);
        retval.Add(count);
        retval.Add(totalPCorrect);
        return retval;
        
    }
}                    
            
