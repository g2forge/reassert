package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import com.g2forge.alexandria.analysis.ISerializableFunction1;

public interface IPartialPatternBuilder<Arguments, Result, Pattern extends IPattern<?>, Built, Builder> {
	public Builder alt(Pattern pattern0, Pattern pattern1, @SuppressWarnings("unchecked") Pattern... patterns);

	public ICharacterClassBuilder<? extends Builder> charClass();

	public default Builder digit(int base) {
		if (base < 1 || base > 10) throw new IllegalArgumentException("We currently only support bases up to 10!");
		return charClass().range('0', (char) ('0' + (base - 1))).build();
	}

	public default IGroupBuilder<Arguments, Result, Pattern, ? extends Builder> group() {
		return group(null, null);
	}

	public IGroupBuilder<Arguments, Result, Pattern, ? extends Builder> group(ISerializableFunction1<? super Result, ?> field, Arguments arguments);

	public Builder named(NamedCharacterClass named);

	public Builder opt();

	public Builder plus();

	public Builder star();

	public Builder text(String text);

	public Builder with(Pattern pattern);
}
