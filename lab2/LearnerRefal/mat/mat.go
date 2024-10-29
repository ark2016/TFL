package mat

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
)

type InitData struct {
	Mode string `json:"mode"`
}

type EquivalenceRequest struct {
	MainPrefixes    string `json:"main_prefixes"`
	NonMainPrefixes string `json:"non_main_prefixes"`
	Suffixes        string `json:"suffixes"`
	Table           string `json:"table"`
}

type EquivalenceResponse struct {
	Response string `json:"response"`
	Type     *bool  `json:"type"`
}

func Init(mode string) error {
	url := "http://localhost:8080/generate"

	data := InitData{
		Mode: mode,
	}
	jsonData, err := json.Marshal(data)
	if err != nil {
		return fmt.Errorf("ошибка при маршаллинге данных: %w", err)
	}

	resp, err := http.Post(url, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return fmt.Errorf("ошибка при отправке запроса: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("ошибка: статус ответа %s", resp.Status)
	}

	fmt.Println("Соединение установлено и запрос на /generate выполнен успешно")
	return nil
}

func MembershipQuery(query string) (int, error) {
	url := "http://localhost:8095/checkWord"
	if query == "" {
		query = "ε"
	}

	data := map[string]string{
		"word": query,
	}
	jsonData, err := json.Marshal(data)
	if err != nil {
		fmt.Println("Ошибка при маршаллинге данных:", err)
		return -1, err
	}

	resp, err := http.Post(url, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		fmt.Println("Ошибка при отправке запроса:", err)
		return -1, err
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		var response map[string]bool
		err = json.NewDecoder(resp.Body).Decode(&response)
		if err != nil {
			fmt.Println("Ошибка при декодировании ответа:", err)
			return -1, err
		}

		if response["response"] {
			return 1, nil
		} else {
			return 0, nil
		}
	}
	return -1, err
}

func EquivalenceQuery(mainPrefixes, nonMainPrefixes, suffixes, table string) (bool, string, error) {
	url := "http://localhost:8095/checkTable"

	requestData := EquivalenceRequest{
		MainPrefixes:    mainPrefixes,
		NonMainPrefixes: nonMainPrefixes,
		Suffixes:        suffixes,
		Table:           table,
	}

	jsonData, err := json.Marshal(requestData)
	if err != nil {
		return false, "", fmt.Errorf("ошибка при маршаллинге данных: %w", err)
	}

	resp, err := http.Post(url, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return false, "", fmt.Errorf("ошибка при отправке запроса: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return false, "", fmt.Errorf("ошибка: статус ответа %s", resp.Status)
	}

	bodyBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return false, "", fmt.Errorf("ошибка при чтении тела ответа: %w", err)
	}
	//fmt.Printf("Тело HTTP-ответа: %s\n", string(bodyBytes))

	var response EquivalenceResponse
	err = json.Unmarshal(bodyBytes, &response)
	if err != nil {
		return false, "", fmt.Errorf("ошибка при декодировании ответа: %w", err)
	}
	if response.Type == nil {
		return true, "", nil
	} else {
		return false, response.Response, nil
	}
}
