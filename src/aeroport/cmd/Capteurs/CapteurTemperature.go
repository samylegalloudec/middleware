package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"math/rand"
	"time"

	mqtt "github.com/eclipse/paho.mqtt.golang"
)

//Structure décrivant le modèle JSON du fichier de configuration
type configuration struct {
	brokerAddress    string
	brokerPort       string
	qualityOfService string
	clientId         string
}

func createClientOptions(brokerURI string, clientId string) *mqtt.ClientOptions {
	opts := mqtt.NewClientOptions()

	opts.AddBroker(brokerURI)

	opts.SetClientID(clientId)
	return opts
}

func connect(brokerURI string, clientId string) mqtt.Client {
	fmt.Println("Trying to connect (" + brokerURI + ", " + clientId + ")...")
	opts := createClientOptions(brokerURI, clientId)
	client := mqtt.NewClient(opts)
	token := client.Connect()
	for !token.WaitTimeout(3 * time.Second) {

	}
	if err := token.Error(); err != nil {
		log.Fatal(err)
	}
	return client
}

//Génère une valeur aléatoire qui dépend de la valeur d'avant
func newRandomValue(previousVal int, maxRange int) int {
	return previousVal + (rand.Intn(1+maxRange*2) - maxRange)
}

//Récupère la configuration depuis un fichier txt
func retrieveConfiguration() configuration {
	data, err := ioutil.ReadFile("test.txt")
	if err != nil {
		fmt.Println(err)
	}

	var c configuration

	c = configuration{
		brokerAddress:    "pas bon",
		brokerPort:       "1883",
		qualityOfService: "0",
		clientId:         "Test",
	}

	json.Unmarshal(data, &c)

	return c
}

func main() {
	fmt.Println("Capteur - Test")

	//client := connect("tcp://localhost:1883", "CapteurPublisher")
	/*
		infiniteLoop := 1
		measureNumber := 0
		for infiniteLoop < 1000 {
			timeToConvert := time.Now()
			timestamp := timeToConvert.String()
			client.Publish("CanalTemperature", 0, false, "Mesure n°"+measureNumber)
			client.Publish("CanalTemperature", 0, false, "Id du capteur : "+measureNumber)
			client.Publish("CanalTemperature", 0, false, "Nature de la mesure : "+measureNumber)
			client.Publish("CanalTemperature", 0, false, "Valeur de la mesure : "+measureNumber)
			client.Publish("CanalTemperature", 0, false, "Timestamp : "+timestamp)
			time.Sleep(10 * time.Second)
			measureNumber++
		}
	*/
	c := retrieveConfiguration()
	fmt.Println(c.brokerAddress)
}
