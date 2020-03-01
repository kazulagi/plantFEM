#include <iostream>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <random>
#include <ctime>

using namespace std;

class Points
{
	public:
	int Node;
	vector<double> x;
	vector<double> y;
	vector<vector<double> > distance;
	double min;//最適な距離
	vector<int> route;//正しいルートを格納する箱

	void input();
	void calc_distance();
	void show_distance();
	void show_coordinates();
	void enumerate();
	void optimal_route_output();
	void greedy();
	void random_order(vector<int> &route);
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
		output<<x[route[i]]<<" "<<y[route[i]]<<" "<<route[i]<<endl;//<<" "<<route[i]<<endl;
	}
	output<<x[route[0]]<<" "<<y[route[0]]<<" "<<route[0]<<endl;//<<" "<<route[0]<<endl;//一周する
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
	do{
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

void Points::greedy()
{
	double tmin;//一時的な最適な距離
	vector<int> troute(Node);//一時的なルートを格納する箱
	int iter=1000000;//Node;//調べる回数

	for(int l=0; l<iter; l++)//スタート位置をn通り調べる
	{
		//順番を決める//
		//満遍なく調べる場合//
		/*troute[0]=l;
		for(int i=1; i<Node; i++)
		{
			if(i==l)
			{
				troute[i]=0;
				continue;
			}
			troute[i]=i;
		}*/
		random_order(troute);//乱数で割り振る

		tmin=0.;
		for(int i=1; i<Node; i++)//点を固定していく、現在の点がi
		{
			double dist=distance[troute[i-1]][troute[i]];
			int number=troute[i];
			int count=0;
			for(int j=i+1; j<Node; j++)//その点から最短の点を探す
			{
				if(dist>distance[troute[i-1]][troute[j]])
				{
					dist=distance[troute[i-1]][troute[j]];
					number=j;
					count+=1;
				}
			}
			if(count>0)//入れ替えがある時だけ順番を交換
			{
				int a=troute[number];
				troute[number]=troute[i];
				troute[i]=a;
			}
			tmin+=dist;

			//毎回の並び替え結果を出力//
			//cout<<i<<"  ";
			//for(int k=0; k<Node; k++) cout<<route[k]<<" ";
			//cout<<endl;
		}
		tmin+=distance[troute[Node-1]][troute[0]];//ループ

		if(l==0)
		{
			min=tmin;
			for(int k=0; k<Node; k++) route[k]=troute[k];
		}

		if(tmin<min)
		{
			min=tmin;
			for(int k=0; k<Node; k++) route[k]=troute[k];
		}
		if(l%2000==0) cout<<l<<" "<<min<<endl;
	}
}

void Points::random_order(vector<int> &troute)//重複しない乱数
{
	//シードで乱数を使いたい場合//
	//int seed=65;
	//mt19937 mt(seed);
	mt19937 mt(static_cast<unsigned int>(time(nullptr)));
	uniform_int_distribution<> uniform(0,Node-1);
	
	int a=uniform(mt);
	troute[0]=a;

	for(int i=1; i<Node; i++)
	{
		if(i==a) continue;
		troute[i]=i;
	}

	//for(int i=0; i<Node; i++) cout<<route[i]<<" ";
}

int main()
{
	Points points;//点群オブジェクトを生成
	points.input();//点数と座標を読み込む
	points.calc_distance();//任意の二点間距離を計算
	//points.enumerate();//全部列挙する
	points.greedy();//貪欲法
	points.optimal_route_output();//答えを出力
	
	return 0;
}
