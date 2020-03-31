package swam
package impl
package JNA
package impl
package annotations

import java.lang.annotation.{Annotation, Retention, RetentionPolicy, Target}

/**
  * @author Javier Cabrera-Arteaga on 2020-03-30
  */
class PublicJNAField(val order: Int = 0) extends annotation.StaticAnnotation;
