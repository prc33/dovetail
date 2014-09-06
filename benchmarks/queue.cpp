#include <stdio.h>
#include <stdlib.h>
#include <mutex>
#include <condition_variable>
#include <deque>
#include <thread>
#include <iostream>
#include <vector>

// http://stackoverflow.com/questions/12805041/c-equivalent-to-javas-blockingqueue
template <typename T>
class queue
{
private:
    std::mutex              d_mutex;
    std::condition_variable d_condition;
    std::deque<T>           d_queue;
public:
    void push(T const& value) {
        {
            std::unique_lock<std::mutex> lock(this->d_mutex);
            d_queue.push_front(value);
        }
        this->d_condition.notify_one();
    }
    T pop() {
        std::unique_lock<std::mutex> lock(this->d_mutex);
        this->d_condition.wait(lock, [=]{ return !this->d_queue.empty(); });
        T rc(std::move(this->d_queue.back()));
        this->d_queue.pop_back();
        return rc;
    }
};

queue<int> data;

void putter(){
  for(int i = 0; i < 1000000; i++) {
    data.push(i);
  }
}

void taker() {
  for(int i = 0; i < 1000000; i++) {
    data.pop();
  }
}

int main(int argc, char **argv) {
#ifdef WOOL
  fprintf(stderr, "ERROR: queue benchmark is not appropriate to wool.\n");
  return 1;
#else
  if(argc < 2) {
    fprintf(stderr, "Usage: queue <n>\n");
#endif
    return 1;
  }

  int n = atoi(argv[1]);

  std::vector<std::thread> threads;

  for(int i = 0; i < n; i++) {
    threads.push_back(std::thread(putter));
    threads.push_back(std::thread(taker));
  }

  for(auto& thread : threads){
    thread.join();
  }

  return 0;
}
