package com.g2forge.reassert.express.v2.model;

import java.util.List;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.core.marker.Helpers;

import lombok.experimental.UtilityClass;

@Helpers
@UtilityClass
public class HExpression {
	@Note(type = NoteType.TODO, value = "Implement isSame using expressions & value system")
	public static boolean isSame(List<? extends IExpression<?, ?>> left, List<? extends IExpression<?, ?>> right) {
		if (left.size() != right.size()) return false;
		for (int i = 0; i < left.size(); i++) {
			if (!left.get(i).isSame(right.get(i))) return false;
		}
		return true;
	}
}
