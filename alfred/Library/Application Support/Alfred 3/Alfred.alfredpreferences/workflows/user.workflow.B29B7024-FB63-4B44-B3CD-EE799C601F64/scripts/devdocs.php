<?php

ini_set('memory_limit', '-1');

use CFPropertyList\CFPropertyList;

require_once 'vendor/autoload.php';
require_once 'workflows.php';

class DevDocs {

    private $workflows;
    private $results;
    private static $baseUrl = 'http://devdocs.io/';
    private static $docUrl = 'http://maxcdn-docs.devdocs.io/';
    private static $cacheDirectory = 'cache/';

    public function __construct($query, $doc) {
        $this->workflows = new Workflows();
        $this->results = array(
            0 => array(),
            1 => array(),
            2 => array()
        );

        $documentations = $this->getDocumentations();
        if (!isset($doc) || empty($doc)) {
            $rootPath = str_replace('/scripts', '', $this->workflows->path());
            $pList = (new CFPropertyList($rootPath.'/info.plist'))->toArray();
            foreach ($pList['connections'] as $key => $value) {
                if (array_key_exists($key, $documentations)) {
                    $this->checkCache($key);
                    $this->processDocumentation($key, $query);
                }
            }
        } else {
            $this->checkCache($doc);
            $this->processDocumentation($doc, $query);
        }
        $this->render();
    }

    private function getDocumentations() {
        $docFile = $this->cacheDirectory.'docs.json';
         // Keep the docs in cache during 7 days
        if (!file_exists($docFile) || (filemtime($docFile) <= time() - 86400 * 7)) {
            file_put_contents($docFile, file_get_contents('http://devdocs.io/docs/docs.json'));
        }
        $docs = json_decode(file_get_contents($docFile));
        $documentations = array();
        foreach ($docs as $doc) {
            $doc->fullName = $doc->name . ($doc->version ? ' '.$doc->version : '');
            $documentations[$doc->slug] = $doc;
        }
        return $documentations;
    }

    private function checkCache ($documentation) {
        if (!file_exists(self::$cacheDirectory)) {
            mkdir(self::$cacheDirectory);
        }
        $docFile = self::$cacheDirectory.$documentation.'.json';
         // Keep the docs in cache during 7 days
        if (!file_exists($docFile) || (filemtime($docFile) <= time() - 86400 * 7)) {
            file_put_contents($docFile, file_get_contents(self::$docUrl.$documentation.'/index.json'));
        }
    }

    private function processDocumentation ($documentation, $query) {

        $query = strtolower($query);
        $data = json_decode(file_get_contents(self::$cacheDirectory.$documentation.'.json'));
        if ($data === NULL) {
            unlink(self::$cacheDirectory.$documentation.'.json');
        }

        $entries = $data->entries;

        $found = array();
        foreach ($entries as $key => $result) {
            $value = strtolower(trim($result->name));
            $description = strtolower(utf8_decode(strip_tags($result->type)));
            
            if (empty($query)) {
                $found[$value] = true;
                $result->documentation = $documentation;
                $this->results[0][] = $result;
            }
            else if (strpos($value, $query) === 0) {
                if (!isset($found[$value])) {
                    $found[$value] = true;
                    $result->documentation = $documentation;
                    $this->results[0][] = $result;
                }
            }
            else if (strpos($value, $query) > 0) {
                if (!isset($found[$value])) {
                    $found[$value] = true;
                    $result->documentation = $documentation;
                    $this->results[1][] = $result;
                }
            }
            else if (strpos($description, $query) !== false) {
                if (!isset($found[$value])) {
                    $found[$value] = true;
                    $result->documentation = $documentation;
                    $this->results[2][] = $result;
                }
            }
        }

    }

    private function render () {
        foreach ($this->results as $level => $results) {
            foreach ($results as $result) {
                $this->workflows->result( $result->name, self::$baseUrl.$result->documentation.'/'.$result->path, $result->name.' ('.$result->type.')', $result->path, $result->documentation.'.png', 'yes',  $result->name);
            }
        }
        echo $this->workflows->toxml();
    }
}

new DevDocs($query, $documentation);
