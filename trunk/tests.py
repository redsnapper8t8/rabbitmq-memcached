#!/usr/bin/env python
import sys, time
import logging

import unittest

import amqplib.client_0_8 as amqp
import memcache

RABBITMQ_HOST = '127.0.0.1'
RABBITMQ_USER = 'guest'
RABBITMQ_PASS = 'guest'
RABBITMQ_EXCHANGE_NAME  = 'test.exchange'
RABBITMQ_QUEUE_NAME     = 'test.queue'

RABBITMC_HOST = '127.0.0.1'
RABBITMC_PORT = 11211

class RabbitMC(unittest.TestCase):
    def setUp(self):
        self.conn = amqp.Connection(RABBITMQ_HOST,
                                    userid=RABBITMQ_USER,
                                    password=RABBITMQ_PASS)
        self.ch = self.conn.channel()        
        self.ch.access_request('/data', active=True, write=True)
        
        self.ch.exchange_declare(RABBITMQ_EXCHANGE_NAME, 'fanout', auto_delete=True)
        self.exchange = RABBITMQ_EXCHANGE_NAME
        self.queue, _, _ = self.ch.queue_declare(RABBITMQ_QUEUE_NAME, auto_delete=True)
        self.queue = self.queue.encode('utf-8')
        self.ch.queue_bind(self.queue, self.exchange)
        
        self.mc = self.createMemcacheClient()
    
    def tearDown(self):
        self.ch.queue_delete(self.queue)
        self.ch.close()
        self.conn.close()
        
    def createMemcacheClient(self):
        return memcache.Client(['%s:%d' % (RABBITMC_HOST, RABBITMC_PORT)], debug=1 if '-v' in sys.argv else 0)
        
    def testGet(self):
        body = 'hello world'
        
        msg = amqp.Message(body, content_type='text/plain')
        
        self.assertEquals(None, self.mc.get(self.queue))
        
        self.ch.basic_publish(msg, self.exchange)
        
        time.sleep(0.1)
                
        self.assertEquals(body, self.mc.get(self.queue))
        self.assertEquals(None, self.mc.get(self.queue))
    
    def testSet(self):
        body = 'hello world'
        
        self.assertEquals(None, self.ch.basic_get(self.queue))
        
        self.assert_(self.mc.set(self.exchange, body))
        
        time.sleep(0.1)
        
        self.assertEquals(body, self.ch.basic_get(self.queue, no_ack=True).body)
        self.assertEquals(None, self.ch.basic_get(self.queue))
    
    def testStats(self):
        stats = self.mc.get_stats()[0][1]
        
        body = 'hello world'
        
        mc = memcache.Client(['127.0.0.1:11211'], debug=1 if '-v' in sys.argv else 0)
        
        self.assertEquals(None, self.mc.get(self.queue))
        self.assert_(self.mc.set(self.exchange, body))
        self.assertEquals(body, self.mc.get(self.queue))
        
        mc = self.createMemcacheClient()
        
        new_stats = mc.get_stats()[0][1]
        
        self.assertEquals(int(stats['cmd_set']) + 1, int(new_stats['cmd_set']))
        self.assertEquals(int(stats['cmd_get']) + 2, int(new_stats['cmd_get']))        
        self.assertEquals(int(stats['get_hits']) + 1, int(new_stats['get_hits']))
        self.assertEquals(int(stats['get_misses']) + 1, int(new_stats['get_misses']))        
        self.assert_(int(stats['bytes_read']) < int(new_stats['bytes_read']))
        self.assert_(int(stats['bytes_written']) < int(new_stats['bytes_written']))
        self.assertEquals(int(stats['curr_connections']) + 1, int(new_stats['curr_connections']))
        self.assertEquals(int(stats['total_connections']) + 1, int(new_stats['total_connections']))
        self.assert_(int(stats['uptime']) > 0)
        self.assert_(int(stats['pid']) > 0)
        self.assert_(int(stats['time']) > 0)
        self.assertEquals('0.1', stats['version'])
        
    def testPerformance(self):
        start = time.clock()
        
        body = ''.join([chr(i) for i in range(256)])
        times = 1000

        for i in range(times):
            self.mc.set(self.exchange, body)
            
        stop = time.clock()
        
        rps = times / stop - start
        
        logging.info("write requests per seconds: %f RPS", rps)
        
        self.assert_(rps > 2000)
        
        for i in range(times-1):
            self.mc.get(self.queue)
            
        self.assertEquals(body, self.mc.get(self.queue))
            
        stop = time.clock()
        
        rps = times / stop - start
        
        logging.info("read requests per seconds: %f RPS", rps)
        
        self.assert_(rps > 500)        

if __name__=='__main__':
    logging.basicConfig(level=logging.DEBUG if '-v' in sys.argv else logging.WARNING,
                        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    
    unittest.main()