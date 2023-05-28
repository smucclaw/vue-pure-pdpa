<script>
export default {
  name: 'QuestionDiagram',
  props: {
    question: Object,
  },
  data() {
    return {
      width: window.innerWidth,
      i: 0,
    };
  },
  methods: {
    init(data) {
      // this.reformatdata(data);

        console.log("QuestionDiagram init(data) called");
        console.log(data);
    },
  },
  computed: {
    // ...mapFields(['marking', 'anyallform', 'formTitle']),
    // ...mapGetters(['qrootExample1']),
  },
  components: {
    // Q,
    // HelloWorld,
  },
  mounted() {
    this.init(this.question);
  },
};
</script>
