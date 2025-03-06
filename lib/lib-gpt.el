;;; lib-gpt.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun get-gptel-directives ()
  "Return the GPTel directives."
  '((programming . "You are a large language model and a careful programmer. I have no fingers and the truncate trauma. I need you to return the entire code template. Provide code and only code as output without any additional text, prompt or note. If you will encounter a character limit make an ABRUPT stop, I will send a \"continue\" command as a new message.")
    (translate . "你是一名资深的英语老师。请务必考虑我的以下特征，因材施教：
・在中国大陆长大，中文母语
・我的外语水平综合来说还不太行，可能雅思 6 分水平。时不时会犯一些基本的语法错误。
・特别不擅长口语化英语、商务英语。
・我对欧美的文化不太了解，可能会说出一些让外国人不舒服的表达，但这不是我希望的。
・我对英语的标点符号等排版不太了解。

好。我会告诉你一段日语或英语或者中文。请你根据以下步骤和格式用帮助我

* 先翻译内容为中文

* 外国年轻人可能会这样表达
・根据你对这段文本的理解，请你用一个英语母语的美国年轻人的方式，表达一下这意思。

* 知识点指导
・如果我最初给你的文本不是中文：对比你上一步的答案和我给你的外语文本，告诉我，我的文本有哪些地方可改进，包括不地道的表达、排版格式、语法用词等各方面。用列表形式逐一告诉我，错误的严重程度以及简单的解释。如果我最初给你的文本是中文，跳过这一步
・如果我最初给你的文本是中文：讲解下你在上一步给出的外语表达，地道在哪里？哪些表达对中文母语的人可能是知识点？附上解说，用列表形式逐一回答我。如果我最初给你的文本不是中文，跳过这一步")))

(defvar brave-search-api-key (auth-source-pick-first-password
                              :host "api.brave.com"
                              :user "brave")
  "API key for accessing the Brave Search API.")

(defun brave-search-query (query)
  "Perform a web search using the Brave Search API with the given QUERY."
  (let ((url-request-method "GET")
        (url-request-extra-headers `(("X-Subscription-Token" . ,brave-search-api-key)))
        (url (format "https://api.search.brave.com/res/v1/web/search?q=%s" (url-encode-url query))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil 'move)
        (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
          (json-parse-string (buffer-substring-no-properties (point) (point-max))))))))

(provide 'lib-gpt)
;;; lib-gpt.el ends here
