#include <iostream>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

class Points
{
	public:
	int Node;
    int Initial;
    int Final;
	vector<double> x;
	vector<double> y;
	vector<vector<double> > distance;
	double min;//最適な距離
	vector<int> route;//正しいルートを格納する箱

	void input();
    void set_start_and_end(int init, int fin);
	void calc_distance();
	void show_distance();
	void show_coordinates();
	void enumerate();
	void optimal_route_output();
};

void Points::calc_distance()//i=jの場合は0
{
	for(int i=0; i<Node; i++)
	{
		for(int j=0; j<Node; j++)
		{
			if(i!=j)
			{
				distance[i][j]=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			}
			else
			{
				distance[i][j]=0.;
			}
		}
	}
}

void Points::set_start_and_end(int init, int fin)
{
    Initial=init;
    Final = fin;
}

void Points::show_distance()
{
	for(int i=0; i<Node; i++)
	{
		for(int j=0; j<Node; j++)
		{
			cout<<i<<" "<<j<<" "<<distance[i][j]<<endl;
		}
	}
}

void Points::show_coordinates()
{
	for(int i=0; i<Node; i++)
	{
		cout<<i<<" "<<x[i]<<" "<<y[i]<<endl;
	}
}

void Points::input()
{
	//点数読み込み//
	ifstream input;
	input.open("points.txt");
	if(input.fail())
	{
		cout<<"No text file"<<endl;
		exit(1);
	}
    
	input>>Node;

    // initalize start and end (by haruka)// 
    Initial= 0;
    Final = 1;
    for(int i=1; i<Node; i++)
    {
        Final = Final * i;
    }
    Final = Final/2;
    cout << "Now total trial is from "<<Initial << " to " << Final<<endl;

	x.resize(Node);
	y.resize(Node);
	distance.resize(Node, vector<double>(Node));
	route.resize(Node);
	
	//座標読み込み//
	for(int i=0; i<Node; i++)
	{
		input>>x[i]>>y[i];
	}
	input.close();
}

void Points::optimal_route_output()
{
	cout<<"optimal route "<<endl;
	for(int i=0; i<Node; i++) cout<<route[i]<<" ";
	cout<<endl;
	cout<<"optimal distance "<<min<<endl;

	ofstream output;
	output.open("solution.txt");
	for(int i=0; i<Node; i++)
	{
		output<<x[route[i]]<<" "<<y[route[i]]<<" "<< route[i]<<endl;
	}
}

void Points::enumerate()
{
	vector<int> order(Node-1);
	for(int i=0; i<Node-1; i++)
	{
		order[i]=i+1;
	}
	
	//next_permutationによる列挙//
	int count=0;
    int nn=1;
	do{
		nn=nn+1;
        //cout << nn << endl;
        if( nn < Initial  )
        {
            continue;
        }
        if( nn > Final  )
        {
            continue;
        }

        if(order[0]<order[Node-2])
		{	
			double dist=0.;
			
			dist+=distance[0][order[0]];//fix
			for(int i=0; i<Node-2; i++)
			{
				dist+=distance[order[i]][order[i+1]];
			}
			dist+=distance[order[Node-2]][0];

			if(count==0)
			{
				min=dist;
				route[0]=0;//fix
				for(int i=0; i<Node-1; i++) route[i+1]=order[i];
				count+=1;
			}
			
			//列挙結果全部出力するとき//
			//cout<<0<<" ";
			//for(int i=0; i<Node-1; i++) cout<<order[i]<<" ";
			//cout<<dist<<endl;
	
			if(dist<min)
			{
				min=dist;
				route[0]=0;//fix
				for(int i=0; i<Node-1; i++) route[i+1]=order[i];
				//cout<<dist<<endl;
			}
		}

	}while(next_permutation(order.begin(),order.end()));
}

int main()
{
    
	Points points;//点群オブジェクトを生成
    int Initial;
    int Final;

    Initial = 1;
    Final = 1000;
    points.input();//点数と座標を読み込む
    //points.set_start_and_end(Initial, Final); // set range of investigation
	points.calc_distance();//任意の二点間距離を計算（距離の対称性を無視）
	points.enumerate();//全部列挙する（円順列であることを無視）
	points.optimal_route_output();//答えを出力
    return 0;
}
