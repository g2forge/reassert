package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.function.IFunction1;

public interface IPatternBuilder<Arguments, Result, Pattern extends IPattern<?>, Built extends IPattern<Result>> extends IPartialPatternBuilder<Arguments, Result, Pattern, Built, IPatternBuilder<Arguments, Result, Pattern, Built>>, IConvertingBuilder<IFunction1<? super IFunction1<ISerializableFunction1<? super Result, ?>, String>, Result>, Built> {
	public default Built build() {
		return build(null);
	}
}
