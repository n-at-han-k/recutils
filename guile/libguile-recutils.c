/* libguile-recutils.c - Guile bindings for librec.  */

/* Copyright (C) 2010-2020 Jose E. Marchesi */

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <rec.h>
#include <libguile.h>


/* Forward declarations. */
void destroy_field(void *field);

/************************ Records *******************************/

/* Free up the resources of a record. */
void destroy_record(void *s)
{
  rec_record_destroy((rec_record_t)s);
}

SCM record_to_scm(rec_record_t rec)
{
  return scm_from_pointer(rec, destroy_record);
}

SCM_DEFINE (scm_new_rec, "%make-empty-rec", 0, 0, 0, (),
            "Make a new empty record.")
{
  rec_record_t rec;

  rec = rec_record_new();
  if (!rec)
    return SCM_BOOL_F;
  return record_to_scm(rec);
}

SCM_DEFINE (scm_get_record_field_ptrs, "%rec-field-ptrs", 1, 0, 0, (SCM ptr),
            "Return all field pointers of the %rec, these are not pointers\n"
            "to the original values.")
{
  rec_record_t rec = (rec_record_t)scm_to_pointer(ptr);

  rec_mset_iterator_t iter;
  rec_mset_elem_t elem;
  rec_field_t field;
  SCM cons = scm_c_eval_string("'()");

  iter = rec_mset_iterator (rec_record_mset (rec));

  while (rec_mset_iterator_next (&iter, MSET_ANY, (const void **) &field, &elem))
    {
      if (rec_mset_elem_type(elem) == MSET_FIELD)
        {
          rec_field_t dup = rec_field_dup (field);
          SCM ptr = scm_from_pointer(dup, destroy_field);
          cons = scm_cons(ptr, cons);
        }
    }

  return scm_reverse(cons);
}

SCM_DEFINE (record_to_alist, "%rec->alist", 1, 0, 0, (SCM ptr), "Return an alist of the record fields.")
{
  rec_record_t rec = (rec_record_t)scm_to_pointer(ptr);

  rec_mset_iterator_t iter;
  rec_mset_elem_t elem;
  rec_field_t field;
  SCM cons = scm_c_eval_string("'()");
  SCM comsym = scm_from_utf8_symbol("comment");

  iter = rec_mset_iterator (rec_record_mset (rec));

  while (rec_mset_iterator_next (&iter, MSET_ANY, (const void **) &field, &elem))
    {
      if (rec_mset_elem_type(elem) == MSET_FIELD)
        {
          const char* name = rec_field_name(field);
          const char* value = rec_field_value(field);
    
          SCM scm_name = scm_from_utf8_string(name);
          SCM scm_value = scm_from_utf8_string(value);
          cons = scm_cons(scm_cons(scm_name, scm_value), cons);
        }
      else
        {
          rec_comment_t comment = (rec_comment_t) field;

          const char *text = rec_comment_text(comment);
          SCM comstr = scm_from_utf8_string(text);
          cons = scm_cons(scm_cons(comsym, comstr), cons);
        }
    }
  
  rec_mset_iterator_free (&iter);

  return scm_reverse(cons);
}

SCM_DEFINE (scm_rec_add_field_value_m, "%rec-add-field-value!", 3, 0, 0, (SCM ptr, SCM name, SCM value),
            "Add a field with value to a rec, modifying the rec in-place.\n"
            "Returns the added field as an instance of <rec-field>.")
{
  SCM_ASSERT_TYPE(scm_is_string(name), name, 1, "rec-add-field-value", "string");
  rec_record_t record = (rec_record_t)scm_to_pointer(ptr);
  rec_field_t field;
  
  const char *fname = scm_to_utf8_string(name);
  const char *str;

  if (scm_is_string(value))
    {
      str = scm_to_utf8_string(value);
    }
  else
    {
      str = scm_to_utf8_string(scm_call_1(scm_c_eval_string("object->string"), value));
    }

  field = rec_field_new (fname, str);
  rec_mset_append(rec_record_mset(record), MSET_FIELD, (void*) field, MSET_ANY);

  return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_rec_add_field_value, "%rec-add-field-value", 3, 0, 0, (SCM ptr, SCM name, SCM value),
           "Add a field to the record, returns a new record with the added field.\n"
           "The old record is left untouched.")
{
  SCM_ASSERT_TYPE(scm_is_string(name), name, 2, "rec-add-field-value", "string");

  rec_record_t record = (rec_record_t)scm_to_pointer(ptr);

  /* Duplicate the record. */
  rec_record_t dup = rec_record_dup (record);

  rec_field_t field;
  const char *fname = scm_to_utf8_string(name);
  const char *str;

  if (scm_is_string(value))
    {
      str = scm_to_utf8_string(value);
    }
  else
    {
      str = scm_to_utf8_string(scm_call_1(scm_c_eval_string("object->string"), value));
    }

  field = rec_field_new (fname, str);
  rec_mset_append(rec_record_mset(dup), MSET_FIELD, (void*) field, MSET_ANY);

  return record_to_scm(dup);
}

SCM_DEFINE (string_to_record, "%string->rec", 1, 0, 0, (SCM s),
            "Parse a string into a recutils record.\n"
            "Returns a new record, or false if the parsing fails.")
{
  SCM_ASSERT_TYPE(scm_is_string(s), s, 1, "string->rec", "string");
  const char* str = scm_to_utf8_string(s);
  rec_parser_t parser = rec_parser_new_str(str, "dummy");
  
  rec_record_t record;
  if (!rec_parse_record(parser, &record))
    {
      return SCM_BOOL_F;
    }
  else
    {
      return record_to_scm(record);
    }  
}

SCM_DEFINE (record_to_string, "%rec->string", 1, 0, 0, (SCM ptr),
            "Return a string representation of the record.")
{
  rec_record_t rec = (rec_record_t)scm_to_pointer(ptr);

  size_t size;
  char* output;
  rec_writer_t writer = rec_writer_new_str(&output, &size);
  rec_writer_set_mode (writer, REC_WRITER_NORMAL);
  rec_write_record(writer, rec);
  rec_writer_destroy(writer);

  return scm_from_utf8_stringn(output, size);
}

/****************** Fields *****************************************/

void destroy_field(void *field)
{
  rec_field_destroy((rec_field_t)field);
}

SCM_DEFINE (scm_field_new, "new-field", 0, 2, 0, (SCM scm_name, SCM scm_value),
            "Make a new field from a string and value.")
{

  const char *name;
  const char *value;

  if (scm_is_eq(scm_name, SCM_UNDEFINED))
    {
      name = "";
    }
  else
    {
      SCM_ASSERT_TYPE(scm_is_string(scm_name), scm_name, 1, "new-field", "string");
      name = scm_to_utf8_string(scm_name);
    }

  if (scm_is_eq(scm_value, SCM_UNDEFINED))
    {
      value = "";
    }
  else
    {
      SCM_ASSERT_TYPE(scm_is_string(scm_value), scm_value, 2, "new-field", "string");
      value = scm_to_utf8_string(scm_value);
    }

  rec_field_t field = rec_field_new (name, value);

  if (!field)
    return SCM_BOOL_F;

  return scm_from_pointer(field, destroy_field);
}


SCM_DEFINE(scm_field_get_name, "%field-name", 1, 0, 0, (SCM ptr),
           "Get the name of a field")
{
  rec_field_t field = (rec_field_t)scm_to_pointer(ptr);

  const char *name = rec_field_name(field);
  
  return scm_from_utf8_string(name);
}

SCM_DEFINE(scm_field_get_value, "%field-value", 1, 0, 0, (SCM ptr),
           "Get the value of a field")
{
  rec_field_t field = (rec_field_t)scm_to_pointer(ptr);

  const char *value = rec_field_value(field);
  
  return scm_from_utf8_string(value);
}

SCM_DEFINE(scm_field_set_name, "set-field-name!", 2, 0, 0, (SCM ptr, SCM scm_name),
           "Set the name of a field")
{
  SCM_ASSERT_TYPE(scm_is_string(scm_name), scm_name, 1, "set-field-name!", "string");
  rec_field_t field = (rec_field_t)scm_to_pointer(ptr);

  const char *name = scm_to_utf8_string(scm_name);

  bool result = rec_field_set_name(field, name);

  return scm_from_bool(result);
}


SCM_DEFINE(scm_field_set_value, "set-field-value!", 2, 0, 0, (SCM ptr, SCM scm_value),
           "Set the value of a field")
{
  SCM_ASSERT_TYPE(scm_is_string(scm_value), scm_value, 1, "set-field-value!", "string");
  rec_field_t field = (rec_field_t)scm_to_pointer(ptr);

  const char *value = scm_to_utf8_string(scm_value);

  bool result = rec_field_set_value(field, value);

  return scm_from_bool(result);
}

SCM_DEFINE(scm_parse_field, "%parse-field", 1, 0, 0, (SCM str),
           "Parse a field".)
{
  SCM_ASSERT_TYPE(scm_is_string(str), str, 1, "%parse-field", "str");

  const char *data = scm_to_utf8_string(str);

  rec_parser_t parser = rec_parser_new_str(data, "lol");

  rec_field_t field;

  if (!rec_parse_field(parser, &field))
    {
      return SCM_BOOL_F;
    }

  return scm_from_pointer(field, destroy_field);
}

SCM_DEFINE(scm_field_to_string, "%field-to-string", 1, 0, 0, (SCM ptr),
           "Parse a field".)
{
  char *output;
  size_t size;

  rec_field_t field = (rec_field_t)scm_to_pointer(ptr);

  return scm_from_utf8_string(rec_write_field_str(field, REC_WRITER_NORMAL));
}

SCM_DEFINE(scm_rec_append_rec_m, "%rec-append-rec!", 2, 0, 0, (SCM ptr1, SCM ptr2),
           "Append a record to a record. Return original record.")
{
  rec_record_t orig = (rec_record_t)scm_to_pointer(ptr1);
  rec_record_t app = (rec_record_t)scm_to_pointer(ptr2);

  rec_record_append(orig, app);

  return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_rec_append_rec, "%rec-append-rec", 2, 0, 0, (SCM ptr1, SCM ptr2),
           "Append a record to a record. Return a new record.")
{
  rec_record_t dup = rec_record_dup((rec_record_t)scm_to_pointer(ptr1));
  rec_record_t app = (rec_record_t)scm_to_pointer(ptr2);

  rec_record_append(dup, app);

  return scm_from_pointer(dup, destroy_record);
}

/************* Record sets **********************/

void destroy_rset(void *rset)
{
  rec_rset_destroy((rec_rset_t)rset);
}

SCM_DEFINE(


/************* Databases ************************/

void destroy_db(void *db)
{
  rec_db_destroy((rec_db_t)db);
}

SCM_DEFINE(scm_new_db, "new-db", 0, 0, 0, (),
           "Make a new database.")
{
  rec_db_t db = rec_db_new();

  if (!db)
    return SCM_BOOL_F;

  return scm_from_pointer(db, destroy_db);
}

SCM_DEFINE(scm_parse_db, "%string->db", 1, 0, 0, (SCM str),
           "Parse a database.")
{
  const char *db_str = scm_to_utf8_string(str);

  rec_parser_t parser = rec_parser_new_str(db_str, "lol");
  rec_db_t db;

  if (!rec_parse_db(parser, &db))
    return SCM_BOOL_F;

  return scm_from_pointer(db, destroy_db);
}


void scm_init_recutils()
{
#ifndef SCM_MAGIC_SNARFER
  #include "libguile-recutils.x"
#endif
}
