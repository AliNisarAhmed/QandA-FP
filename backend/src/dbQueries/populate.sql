INSERT INTO "user" (name)
VALUES
('Ali'),
('Samrah')

INSERT INTO question (title, content, user_id)
VALUES
('Haskell', 'Is Haskell difficult to learn?', 2),
('Elm', 'How to structure an elm SPA?', 1)

INSERT INTO answer (content, question_id, userId)
VALUES
('No, but it requires practice', 1, 1),
('Look into the effect pattern', 2, 2)