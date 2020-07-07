package com.g2forge.reassert.reassert.convert.term;

import java.io.IOException;
import java.util.List;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.Terms;

public class TermsDeserializer extends StdDeserializer<ITerms<?>> {
	private static final long serialVersionUID = 1625399775786284418L;

	protected TermsDeserializer() {
		super(ITerms.class);
	}

	@Override
	public ITerms<?> deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		final Terms.TermsBuilder<ITerm> builder = Terms.builder();
		parser.readValuesAs(new TypeReference<List<StoredTerm>>() {}).next().forEach(st -> builder.term(st.getTerm(), st.getRelation()));
		return builder.build();
	}
}